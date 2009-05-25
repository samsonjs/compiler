# A subset of x86 assembly.
# 
# sjs
# may 2009

ROOT = __FILE__.sub(/\/asm\/text\.rb$/, '') unless defined? ROOT
$LOAD_PATH << ROOT unless $LOAD_PATH.include?(ROOT)

require 'asm/asm'

module Assembler

    # Assembler methods output nasm-friendly x86 asm code, line by
    # line.  This is dead easy and we can trust nasm to compile
    # correct machine code, which isn't trivial.
    class Text < AssemblerBase

      def initialize(platform='linux')
        super
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

      # Define a variable with the given name and size (in dwords).
      def defvar(name, dwords=1)
        unless var?(name)
          @bss << "#{name}: resd #{dwords}\n"
          @vars[name] = name
        else
          STDERR.puts "[warning] attempted to redefine #{name}"
        end
      end

      # Emit a line of code wrapped between a tab and a newline.
      def emit(code, options={})
        tab = options.has_key?(:tab) ? options[:tab] : "\t"
        @code << "#{tab}#{code}\n"
      end

      def label(suffix=nil)
        name = super
        @labels[name] = name
        return name
      end

      def output
        File.read(@templatefile).
          sub("{data}", @data).
          sub("{bss}", @bss).
          sub("{code}", @code)
      end

      def emit_label(name=label)
        emit("#{name}:", :tab => nil)
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

    end
end
