# A very basic x86 assembler library for Ruby.  Generally the
# instructions implemented are the minimum needed by the compiler this
# is written for.  x86 is just too big.
# 
# sjs
# may 2009

module Assembler

  # Define a method named `emit` and include this module.  Calling the
  # assembler methods will output nasm-friendly x86 asm code, line by
  # line.
  module X86

    #####################
    # assembler methods #
    #####################

    def x86_mov(dest, src)
      emit("mov #{dest}, #{src.is_a?(Numeric) ? "0x#{src.to_s(16)}" : src}")
    end

    def x86_movzx(dest, src)
      emit("movzx #{dest}, #{src}")
    end

    def x86_add(dest, src)
      emit("add #{dest}, #{src}")
    end

    def x86_sub(dest, src)
      emit("sub #{dest}, #{src}")
    end

    def x86_imul(op)
      emit("imul #{op}")
    end

    def x86_idiv(op)
      emit("idiv #{op}")
    end

    def x86_inc(op)
      emit("inc #{op}")
    end

    def x86_dec(op)
      emit("dec #{op}")
    end

    def x86_push(reg)
      emit("push #{reg}")
    end

    def x86_pop(reg)
      emit("pop #{reg}")
    end

    def x86_call(label)
      emit("call #{label}")
    end

    def x86_neg(reg)
      emit("neg #{reg}")
    end

    def x86_not(rm32)
      emit("not #{rm32}")
    end

    def x86_xchg(op1, op2)
      emit("xchg #{op1}, #{op2}")
    end

    def x86_and(op1, op2)
      emit("and #{op1}, #{op2}")
    end

    def x86_or(op1, op2)
      emit("or #{op1}, #{op2}")
    end

    def x86_xor(op1, op2)
      emit("xor #{op1}, #{op2}")
    end

    def x86_jz(label)
      emit("jz #{label}")
    end

    def x86_jnz(label)
      emit("jnz #{label}")
    end

    def x86_jmp(label)
      emit("jmp #{label}")
    end

    def x86_jl(label)
      emit("jl #{label}")
    end

    def x86_cmp(a, b)
      emit("cmp #{a}, #{b}")
    end

    def x86_lea(a, b)
      emit("lea #{a}, #{b}")
    end

    def x86_shr(a, b)
      emit("shr #{a}, #{b}")
    end

    def x86_loop(label)
      emit("loop #{label}")
    end

    def x86_int(num)
      emit("int 0x#{num.to_s(16)}")
    end

  end
end
