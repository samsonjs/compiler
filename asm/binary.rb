# A very basic x86 assembler library for Ruby.  Generally the
# instructions implemented are the minimum needed by the compiler this
# is written for.  x86 is just too big.
# 
# sjs
# may 2009

module Assembler

  # Define a method named `emit_byte` and one named `binary_size` and
  # include this module.  Calling the assembler methods will output
  # x86 machine code ... hopefully.  So far it's incomplete and
  # binaries just segfault.
  class Binary

    # This structure allows for x86 registers of all sizes.  The
    # number of the register is the index of the array in which it was
    # found.
    Registers = [ [:eax, :ax, :al], # 0
                  [:ecx, :cx, :cl], # 1
                  [:edx, :dx, :dl], # 2
                  [:ebx, :bx, :bl], # 3
                  [:esp, :sp, :ah], # 4
                  [:ebp, :bp, :ch], # 5
                  [:esi, :si, :dh], # 6
                  [:edi, :di, :bh]  # 7
                ]

    # Regex to match any x86 register name.
    RegisterRegex = '(e?[acdb]x|e?[sb]p|e?[sd]i|[acdb][hl])'

    # Match a literal number in binary, octal, decimal, or hex
    NumberRegex = '(0[xXbB]?)?[0-9a-fA-F]+'

    # Match a variable name.
    NameRegex = '[a-zA-Z][a-zA-Z0-9]*'

    # 0.size gives the real answer, we only do x86 though
    MachineBytes = 4    
    MachineBits = MachineBytes * 8
    MinSigned = -1 * 2**(MachineBits-1)
    MaxSigned = 2**(MachineBits-1) - 1
    MinUnsigned = 0
    MaxUnsigned = 2**MachineBits - 1
    SignedRange = MinSigned..MaxSigned

    

    # Count the bytes that were encoded in the given block.
    def asm
      # stash the current number of bytes written
      instruction_offset = bytes_written
      
      yield
      
      # return the number of bytes written
      bytes_written - instruction_offset
    end


    def emit_dword(num)
      num_to_quad(num).each {|byte| emit_byte(byte)}
    end

    # 0-2: r/m
    # 3-5: reg/opcode
    # 6-7: mod
    # 
    # dest and src are tuples of the form [type, value] where type is
    # any of :reg, :rm32, :imm32.  Max _one_ :rm32 arg per call.
    def emit_modrm(dest, src, override)
      if dest[0] == :reg
        reg = override[:op] || regnum(dest[1])

        # mod == 11 (register content)
        if src[0] == :reg
          mod = 3
          rm = regnum(src[1])

          # mod == 00 (pointer)
        elsif src[0] == :rm32
          mod = 0
          parts = decode_addr(src[1])
          rm = case parts[0]
                 # mod == 00 (direct pointer e.g. [eax])
               when :reg
                 regnum(parts[1])
               when :sib
                 sib = parts[1..-1]
                 4
               when :disp
                 disp = parts[1]
                 5
               end
        end
      elsif src[0] == :reg
        reg = override[:op] || regnum(src[1])
      else
        raise "unsupported mod r/m byte! dest=#{dest} src=#{src}"
      end
      emit_byte((mod << 6) & (reg << 3) & rm)
      emit_sib(sib) if defined? sib
      emit_dword(disp) if defined? disp
    end

    def emit_sib(sib)
      scale, index, base = *sib
      if [1,2,4,8].include?(scale)
        scale = log2(scale)
      else
        raise "unsupported SIB scale: #{scale}, should be [1, 2, 4, 8]"
      end
      emit_byte((scale << 6) & (index << 3) & base)
    end

    def register?(op)
      Registers.each_with_index { |list,i| return i if list.include?(op) }
      nil
    end

    def regnum(op)
      num = register?
      raise "not a register: #{op.inspect}" unless num
      num
    end
    
    def immediate?(op)
      op.is_a?(Numeric) || (op.is_a?(String) && op.match(/^#{NumberRegex}$/))
    end

    def rm32?(op)
      offset?(op) || op.respond_to?(:match) && op.match(/^
      \[
       #{RegisterRegex}                       # base register
       (\+#{RegisterRegex}                    # optional index register
        (\*[1248])?                           # optional scale
       )?
      \]
      $/x)
    end

    # 6 versions of the mov instruction are supported:
    #   1.  mov reg32, immediate32 (0xb8+destreg, imm32)
    #   2.  mov reg32, r/m32 (0x8b, mod r/m, maybe sib)
    #   2a. mov eax, memoffset32 (0xa1, disp32)
    #   3.  mov r/m32, reg32 (0x89, mod r/m, maybe sib)
    #   3a. mov memoffset32, eax (0xa3, disp32)
    #   4.  mov r/m32, immediate32 (0xc7, mod r/m, maybe sib, imm32)
    def x86_mov(dest, src)
      dest = dest[6..-1] if dest.is_a?(String) && dest[0..5] == 'dword '
      src = src[6..-1] if src.is_a?(String) && src[0..5] == 'dword '

      asm do

        # version 1: mov r32, imm32
        if register?(dest) && immediate?(src)
          emit_byte(0xb8 + regnum(dest)) # dest encoded in instruction
          emit_dword(parse_num(src))

          # version 2: mov r32, r/m32
        elsif register?(dest) && rm32?(src)
          # version 2a: mov eax, moffs32
          if dest == :eax && offset?(src)
            emit_byte(0xa1)
            num = decode_addr(src)[1]
            emit_dword(num)
          else
            emit_byte(0x8b)
            emit_modrm([:reg, dest], [:rm32, src])
          end

          # version 3: mov r/m32, r32
        elsif rm32?(dest) && register?(src)
          # version 3a: mov moffs32, eax
          if offset?(dest) && src == :eax
            emit_byte(0xa3)
            num = decode_addr(dest)[1]
            emit_dword(num)
          else
            emit_byte(0x89)
            emit_modrm([:rm32, dest], [:reg, src])
          end

          # version 4: mov r/m32, imm32
        elsif rm32?(dest) && immediate?(src)
          emit_byte(0xc7)
          emit_modrm([:rm32, dest], [:imm32, src], :op => 0)
        else
          puts "rm32?(dest): #{rm32?(dest)}\t\trm32?(src): #{rm32?(src)}"
          puts "register?(dest): #{register?(dest)}\t\tregister?(src): #{register?(src)}"
          puts "immediate?(dest): #{immediate?(dest)}\t\timmediate?(src): #{immediate?(src)}"
          puts "offset?(dest): #{offset?(dest)}\t\toffset?(src): #{offset?(src)}"
          #raise "unsupported mov format: mov #{dest}, #{src}"
          puts "!!! unsupported mov format: mov #{dest}, #{src}"
        end

      end # asm do

    end


    def x86_add(dest, src)
    end

    def x86_sub(dest, src)
    end

    def x86_imul(op)
    end

    def x86_idiv(op)
    end

    def x86_inc(op)
      asm do
        if register?(op)
          emit_byte(0x40 + regnum(op))
        elsif rm32?(op)
          emit_byte(0xff)
          emit_modrm(...)
        else
          raise "unsupported op #{op}, wanted r32 or r/m32"
        end
      end
    end

    def x86_push(reg)
    end

    def x86_cmp(a, b)
    end


    def offset?(addr)
      addr.respond_to?(:match) && addr.match(/^\[(#{NameRegex}|#{NumberRegex})\]$/)
    end

    def decode_addr(addr)
      addr = addr[1..-2]   # strip brackets

      if matches = addr.match(/^#{NameRegex}$/)
        unless loc = @vars[matches[0]]
          raise "undefined variable #{matches[0]}"
        end
        [:disp, @bss_offset + loc]
      elsif matches = addr.match(/^#{NumberRegex}$/)
        [:disp, parse_num(matches[0])]
      elsif addr.index('*')
        bi, scale = *addr.split('*')
        base, index = *bi.split('+')
        [:sib, scale.to_i, index.to_sym, base.to_sym]
      elsif addr.index('+')
        base, index = *addr.split('+')
        [:sib, 1, index.to_sym, base.to_sym]
      else
        [:reg, addr.to_sym]
      end
    end
    
    # Parse a number from a string.  Used by emit_dword.
    def parse_num(str)
      # If it's not a string it's a number, just return it.
      return str unless str.is_a?(String)

      str.downcase!
      base = 10                   # default to base 10
      if str[0, 1] == '0'
        base = case str[1, 1]
               when 'x'
                 16
               when 'b'
                 str.slice!(2..-1)
                 2
               else
                 8
               end
      end
      str.to_i(base)
    end

    # Convert a number to a quad of bytes, discarding excess bits.
    # Little endian!
    def num_to_quad(num)
      [
       num & 0xff,
       (num >>  8) & 0xff,
       (num >> 16) & 0xff,
       (num >> 24) & 0xff
      ]
    end

    def log2(x, tol=1e-13)
      result = 0.0
      
      # Integer part
      while x < 1
        resultp -= 1
        x *= 2
      end
      while x >= 2
        result += 1
        x /= 2
      end
      
      # Fractional part
      fp = 1.0
      while fp >= tol
        fp /= 2
        x *= x
        if x >= 2
          x /= 2
          result += fp
        end
      end 
      result
    end

  end # module Binary
  
end # module Assembler
