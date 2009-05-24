#!/usr/bin/env ruby

ROOT = __FILE__.sub(/\/build\.rb$/, '') unless defined? ROOT

require 'compiler'

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

# input: filename
# output: filename
def compile(filename)
  data, bss, code = nil
  File.open(filename, 'r') do |input|
    compiler = Compiler.new(input)
    data, bss, code = compiler.compile
  end
  asm = interpolate("#{ROOT}/template.asm",
                    :data => data, :bss => bss, :code => code)
  outfile = "#{base(filename)}.asm"
  File.open(outfile, 'w') { |out| out.puts(asm) }
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

def build(filename)
  link( asm( compile(filename) ) )
end

def run(filename)
  filename = "./#{filename}" unless filename.include?('/')
  system(filename)
  return $?.exitstatus
end

main if $0 == __FILE__
