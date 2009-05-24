require 'compiler'
require 'stringio'

def error(msg) STDERR.puts(msg) end

def parse(input)
  compiler = Compiler.new(input)
  compiler.parse                # tuple of [data, bss, code]

rescue ParseError => e
  error("[error] #{e.message}")
  error("[context] #{e.context}")
  # error("Aborting!")
  error(e.caller)
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
            StringIO.new("abc=999\nabc-888\n")
          end
  data, bss, code = *parse(input)
  template = File.read("template.asm")
  asm = interpolate(template, :data => data, :bss => bss, :code => code)
  STDOUT.puts(asm)
end

Blacklist = [:eof?, :many, :whitespace?, :op_char?, :skip_whitespace,
             :any_whitespace?, :skip_any_whitespace, :emit, :indent,
             :newline?, :digit?, :alpha?, :newline?, :get_char,
             :get_op, :op?, :alnum?, :get_name, :get_number]

if false
set_trace_func proc { |event, file, line, id, binding, classname|
  if classname == Compiler    &&
      event != 'line'         &&
      !Blacklist.include?(id) &&
      id.to_s[0,4] != 'x86_'
    printf "%8s %-2d %10s\n", event, line, id
  end
}
end

main(ARGV[0].to_s)
