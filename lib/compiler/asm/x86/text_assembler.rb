# A subset of x86 assembly.
#
# sjs
# may 2009

require 'compiler/asm/text_assembler'

class Compiler
  module ASM
    module X86

      # ASM methods output nasm-friendly x86 asm code, line by
      # line. This is dead easy and we can trust nasm to compile
      # correct machine code, which isn't trivial.
      class TextAssembler < ASM::TextAssembler

        def emit_entry_point
        end

        # Define a constant in the .data section.
        def const(name, value)
          @data << "#{name}\tequ  #{value}"
        end

        # Define a variable with the given name and size in bytes.
        def define_var_impl(name, bytes = nil)
          super(name, bytes)
          dwords = bytes / 4
          @bss << "#{name}: resd #{dwords}\n"
        end

        def output
          File.read(template_filename).
            sub("{data}", @data).
            sub("{bss}", @bss).
            sub("{code}", @code)
        end

        def emit_label(name = label)
          emit("#{name}:", tab: nil)
        end

        def mov(dest, src)
          emit("mov #{dest}, #{src}#{src.is_a?(Numeric) ? " ; 0x#{src.to_s(16)}" : ''}")
        end

        def movzx(dest, src)
          emit("movzx #{dest}, #{src}")
        end

        def add(dest, src)
          emit("add #{dest}, #{src}")
        end

        def sub(dest, src)
          emit("sub #{dest}, #{src}")
        end

        def imul(op)
          emit("imul #{op}")
        end

        def idiv(op)
          emit("idiv #{op}")
        end

        def inc(op)
          emit("inc #{op}")
        end

        def dec(op)
          emit("dec #{op}")
        end

        def push(reg)
          emit("push #{reg}")
        end

        def pop(reg)
          emit("pop #{reg}")
        end

        def call(label)
          emit("call #{label}")
        end

        def leave
          emit("leave")
        end

        def neg(reg)
          emit("neg #{reg}")
        end

        def not(rm32)
          emit("not #{rm32}")
        end

        def xchg(op1, op2)
          emit("xchg #{op1}, #{op2}")
        end

        def and_(op1, op2)
          emit("and #{op1}, #{op2}")
        end

        def or(op1, op2)
          emit("or #{op1}, #{op2}")
        end

        def xor(op1, op2)
          emit("xor #{op1}, #{op2}")
        end

        def jz(label)
          emit("jz #{label}")
        end

        def jnz(label)
          emit("jnz #{label}")
        end

        def jmp(label)
          emit("jmp #{label}")
        end

        def jl(label)
          emit("jl #{label}")
        end

        def cmp(a, b)
          emit("cmp #{a}, #{b}")
        end

        def lea(a, b)
          emit("lea #{a}, #{b}")
        end

        def shr(a, b)
          emit("shr #{a}, #{b}")
        end

        def loop_(label)
          emit("loop #{label}")
        end

        def int(num)
          emit("int 0x#{num.to_s(16)}")
        end

        def cdq
          emit("cdq")
        end

      end

    end
  end
end
