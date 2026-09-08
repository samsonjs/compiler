# A subset of x86 assembly.
#
# sjs
# may 2009

require 'asm/asm'
require 'asm/symtab'
require 'asm/varproxy'

module Assembler

    # Assembler methods output nasm-friendly x86 asm code, line by
    # line.  This is dead easy and we can trust nasm to compile
    # correct machine code, which isn't trivial.
    class Text < AssemblerBase

      ROOT = File.expand_path('..', __dir__)

      # Operand size assumed when nasm can't infer one from a register.
      DefaultOperandSize = :dword

      def initialize(platform)
        super
        @symtab = Symtab.new           # Only used to track names, nasm lays things out.
        @data = ''
        @bss = ''
        @code = ''
        @templatefile = "#{ROOT}/template.#{platform}.asm"
        raise "unsupported platform: #{platform}" unless File.readable?(@templatefile)
      end

      # Define a constant in the .data section.
      def const(name, value)
        @data << "#{name}\tequ  #{value}"
      end

      # Define a variable with the given name and size in bytes.
      def defvar(name, bytes=4)
        unless var?(name)
          @symtab.defvar(name, bytes)
          @bss << "#{name}: resb #{bytes}\n"
        else
          STDERR.puts "[warning] attempted to redefine #{name}"
        end
        return var(name)
      end

      def var(name)
        STDERR.puts "[error] undefined variable #{name}" unless var?(name)
        VariableProxy.new(name)
      end

      def var?(name)
        @symtab.var?(name)
      end

      # Define a variable unless it exists.
      def var!(name, bytes=4)
        var?(name) ? var(name) : defvar(name, bytes)
      end

      # Emit a line of code wrapped between a tab and a newline.
      def emit(code, options={})
        tab = options.has_key?(:tab) ? options[:tab] : "\t"
        @code << "#{tab}#{code}\n"
      end

      def mklabel(suffix=nil)
        @symtab.unique_label(suffix)
      end

      def deflabel(name)
        emit("#{name}:", :tab => nil)
      end

      def output
        File.read(@templatefile).
          sub("{data}", @data).
          sub("{bss}", @bss).
          sub("{code}", @code)
      end

      # Memory operands are arrays, optionally prefixed with a size:
      # [addr] or [:byte, addr].  Everything else is used as is.
      def operand(op, default_size=nil)
        return op.to_s unless op.is_a?(Array)
        size, addr = op.size == 2 ? op : [default_size, op.first]
        "#{size} [#{addr}]".lstrip
      end

      # nasm can only infer the size of a memory operand from a
      # register, so spell it out when there isn't one.
      def operands(*ops)
        default_size = DefaultOperandSize unless ops.any? { |op| op.is_a?(RegisterProxy) }
        ops.map { |op| operand(op, default_size) }.join(', ')
      end

      def instruction(name, *ops)
        emit(ops.empty? ? name.to_s : "#{name} #{operands(*ops)}")
      end

      def mov(dest, src)
        comment = src.is_a?(Numeric) ? " ; 0x#{src.to_s(16)}" : ''
        emit("mov #{operands(dest, src)}#{comment}")
      end

      %w[movzx add sub xchg and_ or_ xor cmp lea shr].each do |name|
        define_method(name) { |dest, src| instruction(name.delete('_'), dest, src) }
      end

      %w[imul idiv inc dec push pop neg not_].each do |name|
        define_method(name) { |op| instruction(name.delete('_'), op) }
      end

      %w[call jmp jc je jg jl jne jng jnl jnz jo js jz loop_].each do |name|
        define_method(name) { |label| emit("#{name.delete('_')} #{label}") }
      end

      %w[cdq leave ret].each do |name|
        define_method(name) { instruction(name) }
      end

      def int(num)
        emit("int 0x#{num.to_s(16)}")
      end

    end
end
