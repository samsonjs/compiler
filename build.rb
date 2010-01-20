#!/usr/bin/env ruby

require 'compiler'
require 'asm/text'
require 'asm/binary'
require 'asm/machosymtab'
require 'asm/machofile'

# usage: build.rb <filename> [elf | macho ] [asm | bin]

DefaultBinFormats = Hash.new('bin')
def binformat(p,f) DefaultBinFormats[p]=f end
binformat 'darwin', 'macho'
binformat 'linux',  'elf'

def main
  filename = ARGV[0].to_s
  raise "can't read #{filename}" unless File.readable?(filename)
  platform = `uname -s`.chomp.downcase
  binformat = ARGV[1] ? ARGV[1].downcase : DefaultBinFormats[platform]
  puts "Building #{filename} for #{platform}, binformat is #{binformat} ..."
  outfile = build(filename, platform, binformat)
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
      compiler = Compiler.new(input, asm)
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
def assemble(filename, binformat='elf')
  f = base(filename)
  outfile = "#{f}.o"
  run_and_warn_on_failure("nasm -f #{binformat} -g -o #{outfile} #{filename} 2>&1")
  return outfile
end

# link with ld, return resulting filename.
def link(filename, platform='linux')
  f = base(filename)
  cmd, args = *case platform
               when 'darwin': ['gcc', '-arch i386']
               when 'linux': ['ld', '']
               else
                 raise "unsupported platform: #{platform}"
               end
  run_and_warn_on_failure("#{cmd} #{args} -o #{f} #{filename} 2>&1")
  `chmod u+x #{f}`
  return f
end

def build(filename, platform='linux', binformat='elf')
  objfile = base(filename) + '.o'
  symtab, objwriter_class =
    case binformat
    when 'elf':   [Assembler::ELFSymtab.new, Assembler::ELFFile]
    when 'macho': [Assembler::MachOSymtab.new, Assembler::MachOFile]
    else
      raise "unsupported binary format: #{binformat}"
    end
  compile(filename, objfile, Assembler::Binary.new(platform, symtab, objwriter_class))
  exefile = link(objfile, platform)
  return exefile
end

def build_asm(filename, platform='linux', binformat='elf')
  asmfile = base(filename) + '.asm'
  compile(filename, asmfile, Assembler::Text.new(platform))
  objfile = assemble(asmfile, binformat)
  exefile = link(objfile, platform)
  return exefile
end

def run(filename)
  filename = "./#{filename}" unless filename.include?('/')
  `#{filename}`
  return $?.exitstatus
end

main if $0 == __FILE__
