#!/usr/bin/env ruby

ROOT = __FILE__.sub(/\/build\.rb$/, '') unless defined? ROOT

require 'compiler'


X86_exit = [0x89, 0xc3,         # mov ebx, eax (exit code)
            0xb8, 1, 0, 0, 0,   # mov eax, 1
            0xcd, 0x80          # int 0x80
           ].pack('c*')

def main
  filename = ARGV[0].to_s
  raise "can't read #{filename}" unless File.readable?(filename)    
end


def error(msg) STDERR.puts(msg) end

# name part (filename minus extension)
def base(filename)
  filename.sub(/\.[^.]*$/, '')
end

def interpolate(templatefile, data)
  template = File.read(templatefile)
  data.inject(template) do |template, mapping|
    token, replacement = *mapping
    template.sub("{#{token}}", replacement)
  end
end

# filename: input filename
# format:   output format, nasm or binary
# returns:  output filename
def compile(filename, format='asm')

  # compile to asm or binary
  output = nil
  File.open(filename, 'r') do |input|
    compiler = Compiler.new(input, format)
    output = compiler.compile
  end
  if format == 'asm'
    mode = 'w'
    data, bss, code = *output
    output = interpolate("#{ROOT}/template.asm",
                         :data => data, :bss => bss, :code => code)
  else
    mode = 'wb'
    output += X86_exit
  end
  outfile = "#{base(filename)}.#{format}"
  File.open(outfile, mode) do |out|
    if format == 'asm'
      out.puts(output)
    end
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
def asm(filename)
  f = base(filename)
  outfile = "#{f}.o"
  output = `nasm -f elf -g -o #{outfile} #{filename}`
  if $?.exitstatus != 0
    raise "nasm failed: #{$?.exitstatus}", output
  end
  return outfile
end

# link with ld, return resulting filename.
def link(filename)
  f = base(filename)
  output = `ld -o #{f} #{filename}`
  if $?.exitstatus != 0
    raise "ld failed: #{$?.exitstatus}", output
  end
  `chmod +x #{f}`
  return f
end

def build(filename, format='asm')
  if format == 'asm'
    link( asm( compile(filename) ) )
  else # binary
    link( compile(filename, format='bin') )
  end
end

def run(filename)
  filename = "./#{filename}" unless filename.include?('/')
  system(filename)
  return $?.exitstatus
end

main if $0 == __FILE__
