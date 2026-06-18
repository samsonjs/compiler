# sjs
# may 2009

require 'compiler/asm/assembler'

class Compiler
  module ASM

    class TextAssembler < Assembler

      def initialize(delegate)
        super(delegate)

        @vars = {}                   # Symbol table, maps names to locations in BSS.
        @data = ''
        @bss = ''
        @code = ''

        unless File.readable?(template_filename)
          raise "unsupported platform/arch: #{delegate.platform}/#{arch.name}"
        end
      end

      def template_filename
        @template_filename ||= File.join(File.dirname(__FILE__), arch.name, "template.#{delegate.platform}.asm")
      end

      # Define a constant
      def const(name, value)
      end

      # Define a variable with the given name and size in bytes.
      def define_var(name, bytes = arch.bytes)
        unless var?(name)
          define_var_impl(name, bytes)
        else
          STDERR.puts "[warning] attempted to redefine #{name}"
        end
      end

      def define_var_impl(name, bytes = arch.bytes)
      end

      def var(name)
        @vars[name]
      end
      alias_method :var?, :var


      # Emit a line of code wrapped between a tab and a newline.
      def emit(code, options = {})
        tab = options.has_key?(:tab) ? options[:tab] : "\t"
        @code << "#{tab}#{code}\n"
      end

      def label(name = nil)
        # FIXME
        name = super
        @labels[name] = name
        return name
      end

      def output
      end

      def emit_label(name = label)
        emit("#{name}:", tab: nil)
      end

    end

  end
end
