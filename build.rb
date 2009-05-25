#!/usr/bin/env ruby

require 'compiler'
require 'asm/text'
require 'asm/binary'

def main
  filename = ARGV[0].to_s
  raise "can't read #{filename}" unless File.readable?(filename)    
end


def error(msg) STDERR.puts(msg) end

# name part (filename minus extension)
def base(filename)
  filename.sub(/\.[^.]*$/, '')
end


# filename: input filename
# asm:      assembler to use
# returns:  output filename
def compile(filename, asm, binformat='elf')

  File.open(filename, 'r') do |input|
    compiler = Compiler.new(input, asm, binformat)
    compiler.compile
  end

  ext = asm.class.name.split('::').last[0,3].downcase == 'bin' ? 'bin' : 'asm'
  outfile = "#{base(filename)}.#{ext}"
  File.open(outfile, 'wb') do |out|
    out.puts(asm.output)
  end
  return outfile

rescue ParseError => e
  error("[error] #{e.message}")
  error("[context] #{e.context}")
  # error("Aborting!")
  error(e.caller)
  exit(1)
end

# assemble using nasm, return resulting filename.
def asm(filename, binformat='elf')
  f = base(filename)
  outfile = "#{f}.o"
  output = `nasm -f #{binformat} -g -o #{outfile} #{filename}`
  if $?.exitstatus != 0
    puts output
    raise "nasm failed: #{$?.exitstatus}"
  end
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
  output = `#{cmd} #{args} -o #{f} #{filename}`
  if $?.exitstatus != 0
    puts output
    raise "ld failed: #{$?.exitstatus}"
  end
  `chmod +x #{f}`
  return f
end

# TODO Use a dependency injection framework for the assembler, and
#      other parts as things become more modular.
def build(filename, platform='linux', format='asm', binformat='elf')
  bin = if format == 'asm'
          code = compile(filename, Assembler::Text.new(platform))
          obj = asm( code, binformat )
          link( obj, platform )
        else # binary
          obj = compile(filename, Assembler::Binary.new(platform), binformat)
          link( obj, platform )
        end
  return bin
end

def run(filename)
  filename = "./#{filename}" unless filename.include?('/')
  system(filename)
  return $?.exitstatus
end

main if $0 == __FILE__
