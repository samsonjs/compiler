module Compiler
  # Drives a source file all the way to an executable: parse it, hand the
  # result to an assembler back-end, then assemble and link.
  module Build
    module_function

    DEFAULT_BIN_FORMATS = Hash.new("bin")
    def binformat(p, f)
      DEFAULT_BIN_FORMATS[p] = f
    end
    binformat "darwin", "macho"
    binformat "linux", "elf"

    # asm: generate assembly and let nasm encode it.
    # bin: encode machine code ourselves and write the object file directly.
    def builder(format)
      case format
      when "asm" then method(:build_asm)
      when "bin" then method(:build)
      else raise "unsupported format: #{format}"
      end
    end

    def error(msg)
      warn(msg)
    end

    # name part (filename minus extension)
    def base(filename)
      filename.sub(/\.[^.]*$/, "")
    end

    # infile:   input filename
    # outfile:  output filename
    # asm:      assembler to use
    def compile(infile, outfile, asm)
      File.open(infile, "r") do |input|
        File.open(outfile, "wb") do |out|
          compiler = Parser.new(input, asm)
          out.print(compiler.compile)
        end
      end
    rescue ParseError => e
      error("[error] #{e.message}")
      error("[context] #{e.context}")
      # error("Aborting!")
      error(e.caller)
      exit(1)
    end

    def run_and_warn_on_failure(command)
      output = `#{command}`
      if $?.exitstatus != 0
        puts
        print output
        name = command.split.first
        raise "#{name} failed: #{$?.exitstatus}"
      end
    end

    # assemble using nasm, return resulting filename.
    def assemble(filename, binformat = "elf")
      f = base(filename)
      outfile = "#{f}.o"
      run_and_warn_on_failure("nasm -f #{binformat} -g -o #{outfile} #{filename} 2>&1")
      outfile
    end

    # link with ld, return resulting filename.
    def link(filename, outdir, platform = "linux")
      f = base(filename)
      cmd, args = *case platform
                   when "darwin"
                     ["gcc", "-arch i386"]
                   when "linux"
                     ["ld", "-m elf_i386"]
                   else
                     raise "unsupported platform: #{platform}"
                   end
      run_and_warn_on_failure("#{cmd} #{args} -o #{f} #{filename} 2>&1")
      `chmod u+x #{f}`
      f
    end

    def build(filename, outdir, platform = "linux", binformat = "elf")
      objfile = File.join(outdir, base(filename) + ".o")
      symtab, objwriter_class =
        case binformat
        when "elf"
          [ASM::ELF::SymbolTable.new, ASM::ELF::ObjectFile]
        when "macho"
          [ASM::MachO::SymbolTable.new, ASM::MachO::ObjectFile]
        else
          raise "unsupported binary format: #{binformat}"
        end
      compile(filename, objfile, ASM::X86::BinaryAssembler.new(platform, symtab, objwriter_class))
      link(objfile, outdir, platform)
    end

    def build_asm(filename, outdir, platform = "linux", binformat = "elf")
      asmfile = File.join(outdir, base(filename) + ".asm")
      compile(filename, asmfile, ASM::X86::TextAssembler.new(platform))
      objfile = assemble(asmfile, binformat)
      link(objfile, outdir, platform)
    end

    RunResult = Data.define(:output, :status)

    def run(filename)
      filename = "./#{filename}" unless filename.include?("/")
      output = `#{filename}`
      RunResult.new(output:, status: $?.exitstatus)
    end
  end
end
