# A very basic x86 assembler library for Ruby.  Generally the
# instructions implemented are the minimum needed by the compiler this
# is written for.  x86 is just too big.
# 
# sjs
# may 2009

require 'asm/asm'

module Assembler

  class Binary < AssemblerBase

    include Registers

    DEBUG_OUTPUT = false

    # 0.size gives the real answer, we only do x86-32 though
    MachineBytes = 4    
    MachineBits = MachineBytes * 8
    MinSigned = -1 * 2**(MachineBits-1)
    MaxSigned = 2**(MachineBits-1) - 1
    MinUnsigned = 0
    MaxUnsigned = 2**MachineBits - 1
    SignedInt = MinSigned..MaxSigned
    SignedByte = -128..127

    # This is used for encoding instructions.  Just as the generated asm
    # contains "BITS 32", binary is generated for 32-bit protected mode.
    DefaultOperandSize = :dword

    SizeMap = {:byte => 8, :word => 16, :dword => 32}

    X86_start = {
      'linux' => [],
      'darwin' => [ 0x55,                  # push ebp
                    0x89, 0xe5,            # mov ebp, esp
                    0x81, 0xec, 8, 0, 0, 0 # sub esp, 8
                  ]
    }

    X86_exit = {
      'linux' => [ 0x89, 0xc3,         # mov ebx, eax (exit code)
                   0xb8, 1, 0, 0, 0,   # mov eax, 1
                   0xcd, 0x80          # int 0x80
                 ],

      'darwin' => [ 0xc9,       # leave
                    0xc3        # ret
                  ]
    }

    attr_reader :eip

    def initialize(platform, symtab, objwriter)
      super(platform)
      @symtab = symtab
      @objwriter = objwriter
      @binary = []                 # Byte array of machine code.
      @eip = 0                     # Our instruction pointer, or the number of bytes written.
      
      # Always include the _main entry point in our symbol table.  It begins at the
      # beginning of the __TEXT segment, 0x0.
      @symtab.deflabel('_main', @eip)
    end

    def output
      resolve_labels
      blobs = X86_start[@platform] + @binary + X86_exit[@platform]
      binary = blobs.pack('c*')
      @objwriter.text(binary)
      @objwriter.const(@symtab.const_data)
      @objwriter.bss(@symtab.bss_size)
      @objwriter.symtab(@symtab)
      @objwriter.serialize
    end

    def resolve_labels
      bytes_read = 0
      @binary.each_with_index do |x, i|
        if x.is_a?(Numeric)
          bytes_read += 1

        elsif addr?(x)
          @binary[i, 1] = x[1..-1]
          bytes_read += 1

        else # label to resolve
          # the actual eip points to the next instruction already, so should we.
          real_eip = bytes_read + 4
          addr = @symtab.lookup_label(x) - real_eip # dest - src to get relative addr
          puts "resolved label: #{x} = 0x#{@symtab.lookup_label(x).to_s(16)} (rel: 0x#{addr.to_s(16)}, eip = 0x#{real_eip.to_s(16)}, bytes_read = 0x#{bytes_read.to_s(16)})" if DEBUG_OUTPUT
          @binary[i, 1] = num_to_quad(addr)
          # count the first byte just written, the rest are counted normally
          bytes_read += 1
        end
      end
    end

    def addr?(x)
      x.is_a?(Array) && x[0] == :addr
    end
    
    def addr_size(addr)
      addr.length - 1
    end
    
    def defconst(name, bytes, value)
      @symtab.defconst(name, bytes, value)
    end

    # Define a variable with the given name and size in bytes.
    def defvar(name, bytes=4)
      unless @symtab.var?(name)
        @symtab.defvar(name, bytes)
      else
        STDERR.puts "[warning] attempted to redefine #{name}"
      end
    end

    # These methods are all delegated to the symbol table.
    %w[var var? const const?].each do |method|
      define_method(method) do |name|
        @symtab.send(method, name)
      end
    end
    

    # Count the bytes that were encoded in the given block.
    def asm
      # stash the current number of bytes written
      instruction_offset = @eip
      
      print "0x#{@eip.to_s(16).rjust(4, '0')}\t" if DEBUG_OUTPUT

      yield
      
      # return the number of bytes written
      @eip - instruction_offset
      
      puts if DEBUG_OUTPUT
    end
    
    
    def emit_byte(byte)

      ##### The joke's on me! Array#pack('c*') already does this.  It is nice to see
      #     in the debugging output though, so it stays for now.
      #
      # Convert negative native ints into signed bytes.
      #
      # Calculate the signed byte as the difference between -1 (0xff) and some
      # number, X.  When byte == -1 we want X == 0, so X == -byte - 1.
      # Since -byte == ~byte + 1, then -byte - 1 == ~byte + 1 - 1 == ~byte,
      # and X == ~byte.  We want the *signed byte* -1, so we use 0xff,
      # *not* -1.  Ruby sees our signed bytes as positive ints 0-255.
      #
      byte = 0xff - ~byte if byte < 0 && byte >= -128

      # make sure it's a byte
      raise "not a byte: #{byte.inspect}" unless byte == byte & 0xff
      
      byte = byte & 0xff      
      ###  end of pointless code
      
      print (byte >= 0 && byte < 0x10 ? '0' : '') + byte.to_s(16) + ' ' if DEBUG_OUTPUT
      
      @binary << byte
      @eip += 1
    end

    def emit_addr(addr)
      @eip += addr.length
      addr.insert(0, :addr)
      puts addr.inspect if DEBUG_OUTPUT
      @binary << addr
    end

    def emit_future_addr(label)
      print "<#{label}> " if DEBUG_OUTPUT
      @binary << label
      @eip += 4   # all jumps are 32-bit relative for now
    end

    def emit_dword(num)
      num_to_quad(num).each { |byte| emit_byte(byte) }
    end
    
    def mklabel(suffix=nil)
      @symtab.unique_label(suffix)
    end

    def emit_label(name)
      puts "\n#{name} (0x#{@eip.to_s(16)}):" if DEBUG_OUTPUT
      @symtab.deflabel(name, @eip)
    end

    def emit_modrm(addr, reg=0)
      mod = 0
      rm = 0
      disp8 = nil
      disp32 = nil
      sib = nil

      # effective address
      if addr.is_a?(Array)
        eff_addr = addr[1] || addr[0] # works with or without size prefix
        raise "invalid effective address: #{addr.inspect}" unless eff_addr
        case eff_addr
        when RegisterProxy

          # Simple register addressing, e.g. [ESI].
          #
          # mod == 00
          if eff_addr.register?
            mod = 0

            # [ESP] and [EBP] can't be encoded directly.  The
            # workaround is to use SIB to emit the code for [ESP+0]
            # and [EBP+0] instead.
            #
            # To emit [ESP+0] we use SIB with scale=1 index=0 base=ESP.
            if eff_addr == ESP
              rm = 4 # SIB
              sib = mk_sib(1, 0, eff_addr)

            # For [EBP+0] we can encode [EBP]+disp8 directly.
            elsif eff_addr == EBP
              mod = 1
              rm = eff_addr.regnum
              disp8 = 0
            else
              rm = eff_addr.regnum
            end

          # Bare displacements, e.g. [32] or [0x1234abcd]
          elsif eff_addr.index? && eff_addr.index.is_a?(Numeric)

            # disp8, mod == 01
            if SignedByte === eff_addr.index
              mod = 1
              disp8 = eff_addr.index

            # disp32, mod == 10  
            elsif SignedRange === eff_addr.index
              mod = 2
              disp32 = eff_addr.index

            else
              raise "address must fit in 32 bits, this doesn't: #{eff_addr.index}"
            end

          # SIB
          elsif eff_addr.index?
            # scale-index-base, mod == 00 and rm == 100
            rm = 4
            sib = mk_sib(eff_addr.scale || 1, eff_addr.index, eff_addr.base)

          else
            raise "unsupported effective address: #{addr.inspect}"
          end

        # disp32, mod == 00
        when Numeric
          mod = 0
          rm = 5  # 101
          disp32 = eff_addr

        else
          raise "unsupported effective address: #{addr.inspect}"
        end

      # register content, mod == 11
      elsif addr.register?
        mod = 3
        rm = addr.regnum

      else
        raise "unsupported effective address: #{addr.inspect}"
      end

      emit_byte((mod << 6) | (reg << 3) | rm)
      emit_byte(sib) if sib
      emit_addr([disp8]) if disp8
      emit_addr(num_to_quad(disp32)) if disp32
    end


    def mk_sib(scale, index, base)
      if [1,2,4,8].include?(scale)
        scale = log2(scale).to_i
      else
        raise "unsupported SIB scale: #{scale}, should be 1, 2, 4, or 8"
      end
      if index == 0
        index = 4
      elsif index.respond_to?(:regnum)
        index = index.regnum
      end
      base = base.regnum if base.respond_to?(:regnum)
      return (scale << 6) | (index << 3) | base
    end


    def register?(op, size=DefaultOperandSize)
      op.is_a?(RegisterProxy) && op.size == size || op.size == SizeMap[size]
    end

    def immediate?(op, size=DefaultOperandSize)
      bits = SizeMap[size] || size
      op.is_a?(Numeric) && op >= -(2 ** bits / 2) && op <= (2 ** bits - 1)
    end

    def rm?(op, size=DefaultOperandSize)
      register?(op, size) || op.is_a?(Array) && (op.size == 1 || op[0] == size)
    end

    def offset?(addr, size=DefaultOperandSize)
      addr.is_a?(Array) && addr[0].is_a?(Numeric)
    end

    def constant?(op)
      immediate?(op) || offset?(op)
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


    # 8 versions of the mov instruction are supported:
    #   1.  mov reg32, immediate32
    #   2a. mov reg32, r/m32
    #   2b. mov eax, memoffset32
    #   3a. mov r/m32, reg32
    #   3b. mov memoffset32, eax
    #   4.  mov r/m32, immediate32
    #   5.  mov r/m8, reg8
    #   6.  mov reg8, r/m8
    def mov(dest, src)
      
      # These 2 are used in the same way, just the name differs to make the
      # meaning clear.  They are 4-byte values that are emited at the end if
      # they are non-nil.  Only one of them will be emited, and if both are
      # non-nil that one is immediate.
      immediate = nil
      offset = nil
      
      # This is an array of arguments to be passed to emit_modrm, if it is set.
      modrm = nil
      
      # version 1: mov r32, imm32
      if register?(dest) && immediate?(src)
        opcode = 0xb8 + dest.regnum # dest encoded in instruction
        immediate = src

      # version 2a: mov r32, r/m32
      elsif register?(dest) && rm?(src)
        # version 2b: mov eax, moffs32
        if dest == EAX && offset?(src)
          opcode = 0xa1
          offset = src[0]
        else
          opcode = 0x8b
          modrm = [src, dest.regnum]
        end

      # version 3a: mov r/m32, r32
      elsif rm?(dest) && register?(src)
        # version 3b: mov moffs32, eax
        if offset?(dest) && src == EAX
          opcode = 0xa3
          offset = dest[0]
        else
          opcode = 0x89
          modrm = [dest, src.regnum]
        end

      # version 4: mov r/m32, imm32
      elsif rm?(dest) && immediate?(src)
        opcode = 0xc7
        modrm = [dest, 0]
        immediate = src
      
      # version 5: mov r/m8, r8
      elsif rm?(dest, :byte) && register?(src, :byte)
        opcode = 0x88
        modrm = [dest, src.regnum]

      # version 6: mov r8, r/m8
      elsif register?(dest, :byte) && rm?(src, :byte)
        opcode = 0x8a
        modrm = [src, dest.regnum]

      else
        # puts "rm?(dest): #{rm?(dest)}\t\trm?(src): #{rm?(src)}"
        # puts "register?(dest): #{register?(dest)}\t\tregister?(src): #{register?(src)}"
        # puts "immediate?(dest): #{immediate?(dest)}\t\timmediate?(src): #{immediate?(src)}"
        # puts "offset?(dest): #{offset?(dest)}\t\toffset?(src): #{offset?(src)}"
        raise "unsupported MOV instruction, #{dest.inspect}, #{src.inspect}"
      end

      asm do
        emit_byte(opcode)
        emit_modrm(*modrm) if modrm
        emit_dword(immediate || offset) if immediate || offset
      end    
    end


    def movzx(dest, src)
      
      # movzx Gv, ??
      if register?(dest)
        
        opcode = case
                 when rm?(src, :byte): 0xb6 # movzx Gv, Eb
                 when rm?(src, :word): 0xb7 # movzx Gv, Ew
                 else
                   raise "unsupported MOVZX instruction, dest=#{dest.inspect} << src=#{src.inspect} >>"
                 end
        asm do
          emit_byte(0x0f)
          emit_byte(opcode)
          emit_modrm(src, dest.regnum)
        end      
      
      else
        
        raise "unimplemented MOVZX instruction, << dest=#{dest.inspect} >> src=#{src.inspect}"
      end
    end


    def xchg(dest, src)
      if dest == EAX && register?(src)
        asm { emit_byte(0x90 + src.regnum) }
      # swap the args if EAX comes last so we only need to handle one case below.
      elsif src == EAX && register?(dest)
        xchg(src, dest)
      elsif rm?(dest) && register?(src)
        asm do
          emit_byte(0x87)
          emit_modrm(dest, src.regnum)
        end
      elsif register?(dest) && rm?(src)
        asm do
          emit_byte(0x87)
          emit_modrm(src, dest.regnum)
        end
      else
        raise "unsupported XCHG instruction, dest=#{dest.inspect} src=#{src.inspect}"
      end
    end

    # convert double to quad (sign-extend EAX into EDX)
    def cdq
      asm { emit_byte(0x99) }
    end


    def add(dest, src)
      # add r/m32, imm8
      if rm?(dest) && immediate?(src, :byte)
        asm do
          emit_byte(0x83)
          emit_modrm(dest, 0)
          emit_byte(src)
        end

      # add r/m32, imm32
      elsif rm?(dest) && immediate?(src)
         asm do
           emit_byte(0x81)
           emit_modrm(dest, 0)
           emit_dword(src)
         end
      
      # add eax, imm32
      elsif dest == EAX && immediate?(src)
        asm do
          emit_byte(0x05)
          emit_dword(src)
        end
      
      # add reg32, r/m32
      elsif register?(dest) && rm?(src)
        asm do
          emit_byte(0x03)
          emit_modrm(src, dest.regnum)
        end
      
      else
        raise "unsupported ADD instruction, dest=#{dest.inspect} src=#{src.inspect}"
      end
    end


    def sub(dest, src)
      # sub r/m32, imm8
      if rm?(dest) && immediate?(src, :byte)
        asm do
          emit_byte(0x83)
          emit_modrm(dest, 5)
          emit_byte(src)
        end
        
      # sub r/m32, imm32
      elsif rm?(dest) && immediate?(src)
        asm do
          emit_byte(0x81)
          emit_modrm(dest, 5)
          emit_dword(src)
        end
      
      # sub r/m32, reg32
      elsif rm?(dest) && register?(src)
        asm do
          emit_byte(0x29)
          emit_modrm(dest, src.regnum)
        end

      # sub reg32, r/m32
      elsif register?(dest) && rm?(src)
        asm do
          emit_byte(0x2b)
          emit_modrm(src, dest.regnum)
        end

      else
        raise "unsupported SUB instruction, dest=#{dest.inspect} src=#{src.inspect}"
      end
    end


    # Signed multiply.
    def imul(*ops)
      case ops.size

      when 1
        group3(ops[0], 5, 'IMUL')

      when 2
        dest, src = ops
        raise "unsupported IMUL instruction, dest=#{dest.inspect} src=#{src.inspect}"

      else
        raise ArgumentError, "IMUL accepts exactly 1 or 2 operands (got #{ops.inspect})"
      end
    end

    # Unsigned multiply.
    def mul(op)
      group3(op, 4, 'MUL')
    end


    # Signed divide.
    def idiv(op)
      group3(op, 7, 'IDIV')
    end

    # Unsigned divide.
    def div(op)
      group3(op, 6, 'DIV')
    end


    def inc(op)
      asm do
        if register?(op)
          emit_byte(0x40 + regnum(op))
        elsif rm?(op)
          # emit_byte(0xff)
          raise "unimplemented"
        else
          raise "unsupported op #{op}, wanted r32 or r/m32"
        end
      end
    end
    
    
    def dec(op)
      if register?(op)
        # dec r16 / dec r32
        asm { emit_byte(0x48 + op.regnum) }
      else
        raise "unsupported DEC instruction, op=#{op.inspect}"
      end
    end


    def shr(op, n)

      # shr r/m??, imm8
      if SignedByte === n

        opcode = register?(op, :byte) ? 0xc0 : 0xc1

        asm do
          emit_byte(opcode)
          emit_modrm(op, 5)
          emit_byte(n)
        end
                  
      else
        raise "unsupported SHR instruction, op=#{op.inspect}, n=#{n.inspect}"
      end

    end


    def and_(dest, src)
      if rm?(dest, 8) && immediate?(src, 8)
        asm do
          emit_byte(0x80)
          emit_modrm(dest, 4)
          emit_byte(src)
        end
      else
        raise "unsupported AND instruction: dest=#{dest.inspect}, src=#{src.inspect}"
      end
    end


    def xor(dest, src)
      # xor r/m32, reg32
      if rm?(dest) && register?(src)
        asm do
          emit_byte(0x31)
          emit_modrm(dest, src.regnum)
        end
        
      else
        raise "unsupported XOR instruction, dest=#{dest.inspect} src=#{src.inspect}"
      end
    end
  
  
    def not_(op)
      group3(op, 2, 'NOT')
    end
  

    def neg(op)
      group3(op, 3, 'NEG')
    end


    def push(op)
      # push reg32
      if register?(op)
        asm { emit_byte(0x50 + op.regnum) }
        
      elsif immediate?(op, :byte)
        asm do
          emit_byte(0x6a)
          emit_byte(op)
        end
        
      elsif immediate?(op)
        asm do
          emit_byte(0x68)
          emit_dword(op)
        end
      
      else
        raise "unsupported PUSH instruction: op=#{op.inspect}"
      end
    end


    def pop(op)
      # pop reg32
      if register?(op)
        asm { emit_byte(0x58 + op.regnum) }
        
      else
        raise "unsupported POP instruction: op=#{op.inspect}"
      end
    end


    def cmp(op1, op2)
      # cmp r/m32, reg32
      if rm?(op1) && register?(op2)
        asm do
          emit_byte(0x39)
          emit_modrm(op1, op2.regnum)
        end
      
      # cmp eax, imm32
      elsif op1 == EAX && immediate?(op2)
        asm do
          emit_byte(0x3d)
          emit_dword(op2)
        end
        
      else
        raise "unsupported CMP instruction: op1=#{op1.inspect} op2=#{op2.inspect}"
      end
    end


    # Only jmp rel32 is supported.
    def jmp(label)
      asm do
        emit_byte(0xe9)
        emit_future_addr(label)
      end
    end

    # These all jump near (rel32).
    JccOpcodeMap = Hash.new { |key| raise "unsupported Jcc instruction: #{key}" }.
                        merge({
      :jc  => 0x82,  # carry            (CF=1)
      :je  => 0x84,  # equal            (ZF=1) --- same as jz
      :jg  => 0x8f,  # greater          (ZF=0 and SF=OF)
      :jl  => 0x8c,  # less than        (SF!=OF)
      :jne => 0x85,  # not equal        (ZF=0) --- same as jnz
      :jng => 0x8e,  # not greater than (ZF=1 or SF!=OF)
      :jnl => 0x8d,  # not less than    (SF=OF)
      :jnz => 0x85,  # not zero         (ZF=0)
      :jo  => 0x80,  # overflow         (OF=1)
      :js  => 0x88,  # sign             (SF=1)
      :jz  => 0x84   # zero             (ZF=1)
    })

    # Only Jcc rel32 is supported.
    def jcc(instruction, label)
      opcode = JccOpcodeMap[instruction]      
      asm do
        emit_byte(0x0f)
        emit_byte(opcode)
        emit_future_addr(label)
      end
    end

    JccOpcodeMap.keys.each do |name|
      define_method(name) do |label|
        jcc(name, label)
      end
    end


    def lea(r32, mem)
      asm do
        emit_byte(0x8d)
        emit_modrm(mem, r32.regnum)
      end
    end


    def int(n)
      asm do
        emit_byte(0xcd)
        emit_byte(n)
      end
    end


    def ret
      asm { emit_byte(0xc3) }
    end


    def leave
      asm { emit_byte(0xc9) }
    end


    # TODO remove this, LOOP sucks ... only accepts a 1-byte signed offset.
    def loop_(label)
      real_eip = @eip + 2 # loop instruction is 2 bytes
      delta = @symtab.lookup_label(label) - real_eip
      unless SignedByte === delta
        raise "LOOP can only jump -128 to 127 bytes, #{label} is #{delta} bytes away"
      end
      
      asm do
        emit_byte(0xe2)
        emit_byte(delta)
      end
    end


    # Opcode group #3.  1-byte opcode, 1 operand (r/m8 or r/m32).
    # 
    # Members of this group are: DIV, IDIV, MUL, IMUL, NEG, NOT, and TEST.
    def group3(op, reg, instruction)
      opcode = 
        if rm?(op, 8)
          0xf6
        elsif rm?(op)
          0xf7
        else
          raise "unsupported #{instruction} instruction: op=#{op.inspect}"
        end

      asm do
        emit_byte(opcode)
        emit_modrm(op, reg)
      end
    end


  end # class Binary
  
end # module Assembler
