require 'compiler/asm/assembler'
require 'compiler/asm/constant_proxy'
require 'compiler/asm/variable_proxy'

class Compiler
  module ASM

    class BinaryAssembler < Assembler

      DEBUG_OUTPUT = false

      attr_reader :ip

      def initialize(delegate)
        super(delegate)

        @symtab = delegate.symbol_table_factory.new

        # Almost a byte array, except for addresses.
        #
        # Addresses take the form [:<type>, <name>]
        # where <type> is one of: var, const, or label
        #
        # NOTE the type is redundant because of VariableProxy#const?
        #      and labels are just strings.
        #
        #      however, we could accept strings for variable names
        #      if we keep the type tag. something to think about.
        @ir = []

        # Our instruction pointer, or the number of bytes written.
        @ip = 0

        # Map locations in the byte array to var proxies so we can
        # resolve address operations on the 2nd pass.
        @proxies = {}

        emit_entry_point
        emit_preamble
      end

      # register for return values
      def return_reg
        raise 'subclasses must override #return_reg'
      end

      def emit_entry_point
      end

      def emit_preamble
        arch.preamble[delegate.platform].each { |byte| emit_byte(byte) }
      end

      def emit_postamble
        arch.postamble[delegate.platform].each { |byte| emit_byte(byte) }
      end

      def output
        emit_postamble

        byte_array = resolve_labels

        #puts "1st pass: " + byte_array.inspect if DEBUG_OUTPUT

        binary = package(byte_array)

        @symtab.calculate_offsets(binary.length)
        if DEBUG_OUTPUT
          puts ">>> text offset:  0x#{@symtab.text_offset.to_s(16)}"
          puts ">>> const offset: 0x#{@symtab.const_offset.to_s(16)}"
          puts ">>> bss offset:   0x#{@symtab.bss_offset.to_s(16)}"
        end

        # Now that we know where everything lies do the 2nd pass
        # calculating and filling in final var and const addresses.
        #
        # outline:
        #  - resolve all variable proxies in @proxies replacing
        #    the placeholder bytes (0xff) with the real address

        bss_offset = @symtab.bss_offset
        const_offset = @symtab.const_offset
        @proxies.each do |i, proxy|
          #puts ">>> Resolving #{proxy.name}" if DEBUG_OUTPUT
          var = @symtab.var(proxy.name)
          base_addr = if proxy.const?
                        const_offset + @symtab.const(proxy.name)
                      else
                        bss_offset + @symtab.var(proxy.name)
                      end
          #puts ">>> Replacing #{byte_array[i,4].map{|x|'0x' + x.to_s(16)}.inspect} with #{num_to_quad(proxy.resolve(base_addr)).map{|x|'0x' + x.to_s(16)}.inspect}" if DEBUG_OUTPUT
          byte_array[i, arch.pointer_size] = num_to_quad(proxy.resolve(base_addr))
        end

        binary = package(byte_array)

        #puts "2nd pass: " + byte_array.inspect if DEBUG_OUTPUT

        objwriter = delegate.object_file_factory.new
        objwriter.text(binary)
        objwriter.const(@symtab.const_data) if @symtab.const_size > 0
        objwriter.bss(@symtab.bss_size) if @symtab.bss_size > 0
        objwriter.reloc(@symtab.reloc_info)
        objwriter.symtab(@symtab)
        objwriter.serialize
      end

      def resolve_labels
        bytes_read = 0
        bytes = []
        @ir.each_with_index do |x, i|
          if x.is_a?(Numeric)
            bytes << x
            bytes_read += 1

          elsif addr?(x)
            # remember this so we can replace the bogus addr later
            @proxies[bytes_read] = x[1]

            # add a relocation entry for this address
            @symtab.reloc(bytes_read)

            # fill in said bogus addr
            bogus_addr = [0xff] * arch.pointer_size
            bytes += bogus_addr
            bytes_read += bogus_addr.length


          # TODO find out if we should calculate addrs as offsets rather than
          #      absolute as they are done now. (ok for Mach-O, maybe not ELF)
          elsif label?(x)
            # the actual eip points to the next instruction already, so should we.
            real_ip = bytes_read + arch.bytes
            name = x[1]
            addr = @symtab.lookup_label(name) - real_ip # dest - src to get relative addr
            #puts "resolved label: #{x} = 0x#{@symtab.lookup_label(name).to_s(16)} (rel: 0x#{addr.to_s(16)}, ip = 0x#{real_ip.to_s(16)}, bytes_read = 0x#{bytes_read.to_s(16)})" if DEBUG_OUTPUT


            addr_bytes = addr_to_bytes(addr)
            bytes += addr_bytes
            bytes_read += addr_bytes.length

          else
            raise "unknown value in the IR at #{bytes_read} - #{x.inspect}"
          end
        end

        return bytes
      end

      def package(bytes)
        bytes.pack('c*')
      end

      # Silly semantics, but labels don't count as an address since they
      # don't need to be deferred.
      def addr?(x)
        x.is_a?(Array) && [:var, :const].include?(x[0])
      end

      def label?(x)
        x.is_a?(Array) && x[0] == :label
      end

      # XXX this should probably evaluate the value somehow
      def define_const(name, bytes, value)
        @symtab.define_const(name, bytes, value)
        return const(name)
      end

      # Define a variable with the given name and size in bytes.
      def define_var(name, bytes = arch.word_bytes)
        unless @symtab.var?(name)
          @symtab.define_var(name, bytes)
        else
          STDERR.puts "[warning] attempted to redefine #{name}"
        end
        return var(name)
      end

      def var(name)
        STDERR.puts "[error] undefined variable #{name}" unless var?(name)
        VariableProxy.new(name)
      end

      def const(name)
        STDERR.puts "[error] undefined constant #{name}" unless const?(name)
        ConstantProxy.new(name)
      end

      def var?(name)
        @symtab.var?(name)
      end

      def const?(name)
        @symtab.const?(name)
      end

      # Define a variable unless it exists.
      def var!(name, bytes = arch.word_bytes)
        if var?(name)
          var(name)
        else
          define_var(name, bytes)
        end
      end

      # Count the bytes that were encoded in the given block.
      def asm
        # stash the current number of bytes written
        instruction_offset = @ip

        print "0x#{@ip.to_s(16).rjust(4, '0')}\t" if DEBUG_OUTPUT

        yield

        # return the number of bytes written
        @ip - instruction_offset

        puts if DEBUG_OUTPUT
      end


      def emit_byte(byte)

        ##### The joke's on me! Array#pack('c*') already does this. It is nice to see
        #     in the debugging output though, so it stays for now.
        #
        # Convert negative native ints into signed bytes.
        #
        # Calculate the signed byte as the difference between -1 (0xff) and some
        # number, X. When byte == -1 we want X == 0, so X == -byte - 1.
        # Since -byte == ~byte + 1, then -byte - 1 == ~byte + 1 - 1 == ~byte,
        # and X == ~byte. We want the *signed byte* -1, so we use 0xff,
        # *not* -1. Ruby sees our signed bytes as positive ints 0-255.
        #
        byte = 0xff - ~byte if byte < 0 && byte >= -128

        # make sure it's a byte
        raise "not a byte: #{byte.inspect}" unless byte == byte & 0xff

        byte = byte & 0xff
        ###  end of pointless code

        print (byte >= 0 && byte < 0x10 ? '0' : '') + byte.to_s(16) + ' ' if DEBUG_OUTPUT

        @ir << byte
        @ip += 1
      end

      # addresses are emited as arrays of bytes, prefixed with :var, :const, or :label
      def emit_addr(type, name)
        placeholder = [type, name]
        puts placeholder.inspect if DEBUG_OUTPUT
        @ir << placeholder

        # addresses are a constant size
        @ip += arch.pointer_bytes
      end

      def emit_var(name_or_proxy)
        proxy = name_or_proxy.is_a?(VariableProxy) ? name_or_proxy : var(name_or_proxy)
        emit_addr(:var, proxy)
      end

      def emit_const(name)
        proxy = name_or_proxy.is_a?(VariableProxy) ? name_or_proxy : const(name_or_proxy)
        emit_addr(:const, proxy)
      end

      def emit_label(name)
        print "<#{name}> " if DEBUG_OUTPUT
        emit_addr(:label, name)
      end

      def make_label(suffix = nil)
        @symtab.unique_label(suffix)
      end

      def define_label(name)
        puts "\n#{name} (0x#{@ip.to_s(16)}):" if DEBUG_OUTPUT
        @symtab.define_label(name, @ip)
      end

      def addr_to_bytes
        if big_endian?
          num_to_big_endian
        elsif little_endian?
          num_to_little_endian
        else
          raise 'oops'
        end
      end

      # Convert a number to an array of bytes, discarding excess bits.
      def num_to_big_endian(num)
        case arch.pointer_size
        when 4
          [
            # high
            (num >> 16) & 0xff,
            (num >> 24) & 0xff,

            # low
            num & 0xff,
            (num >>  8) & 0xff
          ]
        else
          raise 'unimplemented'
        end
      end

      # Convert a number to an array of bytes, discarding excess bits.
      def num_to_little_endian(num)
        bytes = num_to_big_endian
        bytes.each_slice(2).to_a.reverse.flatten
      end

    end

  end
end
