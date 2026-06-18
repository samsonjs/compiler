# sjs
# may 2009

class Compiler
  module ASM

    # Abstract class for common functionality between different code
    # generators. Also defines somewhat of an interface that must be
    # implemented to be useful.
    class Assembler

      def initialize(delegate)
        @delegate = delegate
      end

      def arch
        delegate.arch
      end

      def block(*args, &block)
        instance_eval(&block)
      end

      def load(n)
      end

      def load_var(name)
      end

      def store_var(name, reg)
      end

      def neg(reg)
      end

      def stack_add(reg)
      end

      def stack_sub(reg)
      end

      def stack_mul_signed(reg)
      end

      def stack_div(reg)
      end

      def stack_or(reg)
      end

      def stack_xor(reg)
      end

      def stack_and(reg)
      end

      def not_(reg)
      end
      alias_method :not, :not_

      def compare(reg, n)
      end

      def je(label)
      end

      def jne(label)
      end

      def jmp(label)
      end

      def mov_reg_imm(reg, n)
      end

      def call(label)
      end

    end

  end
end
