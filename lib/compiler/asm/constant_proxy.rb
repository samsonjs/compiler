class Compiler
  module ASM

    class ConstantProxy < VariableProxy

      def const?
        true
      end

    end

  end
end
