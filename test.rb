require 'compiler'
require 'stringio'

def error(msg) STDERR.puts(msg) end

def parse(input)
  compiler = Compiler.new(input)
  compiler.parse                # tuple of [data, bss, code]

rescue ParseError => e
  error("[error] #{e.message}")
  error("Aborting!")
  exit(1)
end

def interpolate(template, data)
  data.inject(template) do |template, mapping|
    token, replacement = *mapping
    template.sub("{#{token}}", replacement)
  end
end

def main(arg)
  input = if File.readable?(arg)
            File.open(arg)
          else
            # StringIO.new("5*(3-5)*2+2-9/3-8/2-4*(5+5+5)\n")
            StringIO.new("abc=9\nabc-9\n")
          end
  data, bss, code = *parse(input)
  template = File.read("template.asm")
  asm = interpolate(template, :data => data, :bss => bss, :code => code)
  File.open("test.asm", "w") { |f| f.puts(asm) }
end

main(ARGV[0].to_s)
