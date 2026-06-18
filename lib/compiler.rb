this_dir = File.dirname(__FILE__)
Dir.chdir(File.expand_path('..', this_dir))
$LOAD_PATH.unshift(this_dir) unless $LOAD_PATH.include?(this_dir)

require 'compiler/parser'

class Compiler

  attr_reader :platform, :arch_name, :format, :binformat

  attr_reader :arch, :asm, :symbol_table_factory, :object_file_factor

  # platform [String] "linux" or "darwin"
  # arch_name [String] "x86" or "arm"
  # format [String] "text" or "bin"
  # binform [String, nil] "elf" or "macho", only used when format is "bin"
  def initialize(platform, arch_name, format, binformat = nil)
    @platform = platform
    @arch_name = arch_name
    @format = format
    @binformat = binformat
    wire
  end

  def compile(input)
    parser = Parser.new(input, asm)
    parser.parse
    parser.compile
  end


  #######
  private
  #######

  def wire
    if format == 'bin'
      case binformat

      when 'elf'
        wire_elf

      when 'macho'
        wire_macho

      else
        raise "unsupported binary format: #{binformat}"
      end
    end

    case @arch_name

    when 'x86'
      wire_x86

    when 'arm'
      wire_arm

    else
      raise "unsupported arch: #{arch}"
    end
  end

  def wire_elf
    require 'compiler/asm/elf/object_file'
    require 'compiler/asm/elf/symbol_table'

    @symbol_table_factory = ASM::ELF::SymbolTable
    @object_file_factory = ASM::ELF::ObjectFile
  end

  def wire_macho
    require 'compiler/asm/macho/object_file'
    require 'compiler/asm/macho/symbol_table'

    @symbol_table_factory = ASM::MachO::SymbolTable
    @object_file_factory = ASM::MachO::ObjectFile
  end

  def wire_arm
    require 'compiler/asm/arm/binary_assembler'
    require 'compiler/asm/arm/text_assembler'

    @arch = ASM::ARM::Arch.instance
    @asm =
      case format
      when 'text'
        ASM::ARM::TextAssembler.new(self)

      when 'bin'
        ASM::ARM::BinaryAssembler.new(self)

      else
        raise "unsupported output format: #{format}"
      end
  end

  def wire_x86
    require 'compiler/asm/x86/binary_assembler'
    require 'compiler/asm/x86/text_assembler'

    @arch = ASM::X86::Arch.instance
    @asm =
      case format
      when 'text'
        ASM::X86::TextAssembler.new(self)

      when 'bin'
        ASM::X86::BinaryAssembler.new(self)

      else
        raise "unsupported output format: #{format}"
      end
  end

end
