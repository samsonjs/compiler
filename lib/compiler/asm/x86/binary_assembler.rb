# A very basic x86 assembler library for Ruby. Generally the
# instructions implemented are the minimum needed by the compiler this
# is written for. x86 is just too big.
#
# sjs
# may 2009
#
# Refer to the Intel[1] or AMD documentationon on x86 for explanations
# of Mod-R/M encoding, the Scale-Index-Base (SIB) byte, opcode groups.
#
# The start and exit shell codes were obtained by disassembling
# minimal binaries on the respective platforms.

require 'json'
require 'compiler/asm/binary_assembler'
require 'compiler/asm/x86/arch'
require 'compiler/asm/x86/registers'

class Compiler
  module ASM
    module X86

      class BinaryAssembler < ASM::BinaryAssembler

        include Registers

        DEBUG_OUTPUT = false

        SIGNED_BYTE = -128..127

        # This is used for encoding instructions. Just as the equivalent
        # assembly would contain "BITS 32", binary is generated for 32-bit
        # protected mode.
        DEFAULT_OPERAND_SIZE = :dword

        SIZE_MAP = {
          byte: 8,
          word: 16,
          dword: 32
        }

        def emit_entry_point
          # Always include the _main entry point in our symbol table. It begins at the
          # beginning of the __TEXT segment, 0x0.
          define_label('_main')
        end

        # register for return values
        def return_reg
          EAX
        end


        ### Virtual ISA used by parser.

        def load(n)
          mov(return_reg, n)
        end

        def load_var(name)
          mov(return_reg, [var(name)])
        end

        def store_var(name, reg)
          mov([name], reg)
        end


        # stack_* methods expect op1 on the stack

        def stack_add(reg)
          pop(EBX)
          add(reg, EBX)
        end

        def stack_sub(reg)
          pop(EBX)
          sub(reg, EBX)
        end

        def stack_mul_signed(reg)
          pop(EBX)
          imul(EBX)
        end

        def stack_div(reg)
          pop(EBX)                # Get op1
          xchg(reg, EBX)          # Swap the divisor and dividend into
                                  # the correct places.

          # idiv uses edx:eax as the dividend so we need to ensure that edx
          # is correctly sign-extended w.r.t. eax.
          cdq                     # Sign-extend eax into edx (Convert Double to Quad).

          idiv(EBX)               # Divide a (eax) by b (ebx).
        end

        def stack_or(reg)
          pop(EBX)
          self.or(reg)
        end

        def stack_xor(reg)
          pop(EBX)
          xor(reg)
        end

        def stack_and(reg)
          pop(EBX)
          self.and(reg)
        end

        def compare(reg, n)
          cmp(reg, n)
        end

        def mov_reg_imm(reg, imm)
          mov(reg, imm)
        end


        ############################
        ### Instruction Encoding ###
        ############################

        def emit_dword(num)
          num_to_quad(num).each { |byte| emit_byte(byte) }
        end

        def emit_modrm(addr, reg = 0)
          mod = 0
          rm = 0
          disp8 = nil
          disp32 = nil
          sib = nil
          var = nil # variable proxy

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

                # [ESP] and [EBP] can't be encoded directly. The
                # workaround is to use SIB to emit the code for [ESP+0]
                # and [EBP+0] instead.
                #
                # To emit [ESP+0] we use SIB with scale=1 index=0 base=ESP.
                if eff_addr == ESP
                  rm = 4 # SIB
                  sib = make_sib(1, 0, eff_addr)

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
                if SIGNED_BYTE === eff_addr.index
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
                sib = make_sib(eff_addr.scale || 1, eff_addr.index, eff_addr.base)

              else
                raise "unsupported effective address: #{addr.inspect}"
              end

            # disp32, mod == 00
            when Numeric
              mod = 0
              rm = 5  # 101
              disp32 = eff_addr

            when VariableProxy
              mod = 0
              rm = 5
              var = eff_addr

            else
              raise "unsupported effective address: #{addr.inspect}"
            end

          # register content, mod == 11
          elsif addr.register?
            mod = 3
            rm = addr.regnum

          # XXX TODO elsif addr.respond_to?(:name)
          #          (VariableProxy) => [:(var|const), addr.name]
          #
          # i.e. a pointer to that var

          else
            raise "unsupported effective address: #{addr.inspect}"
          end

          emit_byte((mod << 6) | (reg << 3) | rm)
          emit_byte(sib) if sib

          emit_byte(disp8) if disp8

          emit_dword(disp32) if disp32
          emit_var(var) if var
        end


        def make_sib(scale, index, base)
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


        def register?(op, size = DEFAULT_OPERAND_SIZE)
          op.is_a?(RegisterProxy) && op.size == size ||
            op.respond_to?(:size) && op.size == SIZE_MAP[size]
        end

        def immediate?(op, size = DEFAULT_OPERAND_SIZE)
          bits = SIZE_MAP[size] || size
          op.is_a?(Numeric) && op >= -(2 ** bits / 2) && op <= (2 ** bits - 1)
        end

        # Return true if op is a valid operand of the specified size.
        #     (:byte, :word, :dword)
        #
        # Valid operands are:
        #
        #   * registers
        #
        #   * effective addresses (wrapped in an array to look like nasm code)
        #
        # XXX This method is pretty ugly.
        def rm?(op, size = DEFAULT_OPERAND_SIZE)
          is_register = register?(op, size)

          if op.is_a?(Array)
            case op.size

            # [register/memory]
            when 1
              is_reg_or_mem = [Numeric, RegisterProxy, VariableProxy].include?(op[0].class)

            # [<size>, memory]
            when 2
              is_size_and_mem = op[0] == size && [Numeric, RegisterProxy, VariableProxy].include?(op[1].class)

            end

          else
            is_reg_or_mem = false
            is_size_and_mem = false
          end

          is_register || is_reg_or_mem || is_size_and_mem
        end

        def offset?(addr, size = DEFAULT_OPERAND_SIZE)
          addr.is_a?(Array) && (addr[0].is_a?(Numeric) || addr[0].is_a?(VariableProxy))
        end

        def constant?(op)
          immediate?(op) || offset?(op)
        end

        def log2(x, tol = 1e-13)
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


        # 9 versions of the mov instruction are supported:
        #   1. mov reg32, immediate32
        #   2a. mov reg32, r/m32
        #   2b. mov eax, memoffset32
        #   3a. mov r/m32, reg32
        #   3b. mov memoffset32, eax
        #   4. mov r/m32, immediate32
        #   5. mov r/m8, imm8
        #   6. mov reg8, r/m8
        #   7. mov r/m8, reg8
        def mov(dest, src)

          # These 2 are used in the same way, just the name differs to make the
          # meaning clear. They are 4-byte values that are emited at the end if
          # they are non-nil. Only one of them will be emited, and if both are
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

          # version 5: mov r/m8, imm8
          #
          # It's important that this check is first because src integers can
          # pass the register? check in version 7.
          elsif rm?(dest, :byte) && immediate?(src, :byte)
            opcode = 0xc6
            modrm = [dest, 0]
            immediate_byte = src

          # version 6: mov r8, r/m8
          elsif register?(dest, :byte) && rm?(src, :byte)
            opcode = 0x8a
            modrm = [src, dest.regnum]

          # version 7: mov r/m8, r8
          elsif rm?(dest, :byte) && register?(src, :byte)
            opcode = 0x88
            modrm = [dest, src.regnum]

          else
            # puts "rm?(dest): #{rm?(dest)}\t\trm?(src): #{rm?(src)}"
            # puts "register?(dest): #{register?(dest)}\t\tregister?(src): #{register?(src)}"
            # puts "immediate?(dest): #{immediate?(dest)}\t\timmediate?(src): #{immediate?(src)}"
            # puts "offset?(dest): #{offset?(dest)}\t\toffset?(src): #{offset?(src)}"
            # puts "rm?(dest, :byte): #{rm?(dest)}\t\trm?(src, :byte): #{rm?(src, :byte)}"
            # puts "immediate?(dest, :byte): #{immediate?(dest)}\t\timmediate?(src, :byte): #{immediate?(src, :byte)}"
            raise "unsupported MOV instruction, #{dest.inspect}, #{src.inspect}"
          end

          dword = immediate || offset

          asm do
            emit_byte(opcode)
            emit_modrm(*modrm) if modrm

            if dword.is_a?(VariableProxy)
              if dword.const?
                emit_const(dword)
              else
                emit_var(dword)
              end

            elsif dword
              emit_dword(dword)

            elsif immediate_byte
              emit_byte(immediate_byte)

            end
          end
        end


        def movzx(dest, src)

          # movzx Gv, ??
          if register?(dest)

            opcode = case
                     when rm?(src, :byte)
                       0xb6 # movzx Gv, Eb
                     when rm?(src, :word)
                       0xb7 # movzx Gv, Ew
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
            # dec reg32
            asm { emit_byte(0x48 + op.regnum) }
          else
            raise "unsupported DEC instruction, op=#{op.inspect}"
          end
        end


        def shr(op, n)

          # shr r/m??, imm8
          if SIGNED_BYTE === n

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
          if rm?(dest) && register?(src)
            asm do
              emit_byte(0x21)
              emit_modrm(dest, src.regnum)
            end
          elsif rm?(dest, 8) && immediate?(src, 8)
            asm do
              emit_byte(0x80)
              emit_modrm(dest, 4)
              emit_byte(src)
            end
          else
            raise "unsupported AND instruction: dest=#{dest.inspect}, src=#{src.inspect}"
          end
        end
        alias_method :and, :and_

        def or_(dest, src)
          if rm?(dest) && register?(src)
            asm do
              emit_byte(0x9)
              emit_modrm(dest, src.regnum)
            end
          elsif rm?(dest, 8) && immediate?(src, 8)
            asm do
              emit_byte(0x80)
              emit_modrm(dest, 1)
              emit_byte(src)
            end
          else
            raise "unsupported OR instruction: dest=#{dest.inspect}, src=#{src.inspect}"
          end
        end
        alias_method :or, :or_

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
        alias_method :not, :not_


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
            emit_label(label)
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
            emit_label(label)
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


        # NOTE: LOOP only accepts a 1-byte signed offset. Don't use it.
        def loop_(label)
          real_ip = ip + 2 # loop instruction is 2 bytes
          delta = @symtab.lookup_label(label) - real_ip
          unless SIGNED_BYTE === delta
            raise "LOOP can only jump -128 to 127 bytes, #{label} is #{delta} bytes away"
          end

          asm do
            emit_byte(0xe2)
            emit_byte(delta)
          end
        end
        alias_method :loop, :loop_


        # Opcode group #3. 1-byte opcode, 1 operand (r/m8 or r/m32).
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


      end

    end
  end
end
