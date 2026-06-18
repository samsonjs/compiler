#!/usr/bin/env ruby

require 'compiler'

# usage: build.rb <filename> [output filename] [elf | macho] [asm | bin]

BIN_FORMATS = Hash.new('bin')
BIN_FORMATS['darwin'] = 'macho'
BIN_FORMATS['linux'] = 'elf'

def main
  filename = ARGV.shift.to_s
  raise "can't read #{filename}" unless File.readable?(filename)
  outdir = ARGV.shift || '.'
  platform = `uname -s`.chomp.downcase
  binformat = ARGV[1] ? ARGV[1].downcase : BIN_FORMATS[platform]
  puts "Building #{filename} for #{platform}, binformat is #{binformat} ..."
  outfile = build(filename, outdir, platform, binformat)
  puts outfile
  exit
end


def error(msg) STDERR.puts(msg) end

# name part (filename minus extension)
def base(filename)
  filename.sub(/\.[^.]*$/, '')
end


# infile:   input filename
# outfile:  output filename
# asm:      assembler to use
def compile(infile, outfile, asm)

  File.open(infile, 'r') do |input|
    File.open(outfile, 'wb') do |out|
      out.print(Compiler.compile(input, asm))
    end
  end

rescue ParseError => e
  error("[error] #{e.message}")
  error("[context] #{e.context}")
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

# link with ld, return resulting filename.
def link(filename, outdir, platform = 'linux')
  f = base(filename)
  cmd, args = *case platform
               when 'darwin'
                 ['gcc', '-arch i386']
               when 'linux'
                 ['ld', '']
               else
                 raise "unsupported platform: #{platform}"
               end
  run_and_warn_on_failure("#{cmd} #{args} -o #{f} #{filename} 2>&1")
  `chmod u+x #{f}`
  return f
end

def build(filename, outdir, platform = 'linux', binformat = 'macho')
  objfile = File.join(outdir, base(filename) + '.o')
  symtab, objwriter_class =
    case binformat
    when 'elf'
      [Compiler::ELF::SymbolTable.new, Compiler::ELF::ObjectFile]
    when 'macho'
      [Compiler::MachO::SymbolTable.new, Compiler::MachO::ObjectFile]
    else
      raise "unsupported binary format: #{binformat}"
    end
   compile(filename, objfile, Compiler::ASM::Binary.new(platform, symtab, objwriter_class))
  exefile = link(objfile, outdir, platform)
  return exefile
end

def build_asm(filename, outdir, platform = 'linux', binformat = 'macho')
  asmfile = File.join(outdir, base(filename) + '.asm')
  compile(filename, asmfile, Compiler::ASM::Text.new(platform))
  objfile = assemble(asmfile, binformat)
  exefile = link(objfile, platform)
  return exefile
end

# assemble using nasm, return resulting filename.
def assemble(filename, binformat = 'macho')
  f = base(filename)
  outfile = "#{f}.o"
  run_and_warn_on_failure("nasm -f #{binformat} -g -o #{outfile} #{filename} 2>&1")
  return outfile
end

main if $0 == __FILE__
