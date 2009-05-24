require 'interpreter'
require 'stringio'

def error(msg) STDERR.puts(msg) end

def eval(input)
  interpreter = Interpreter.new(input)
  interpreter.run

rescue ParseError => e
  error("[error] #{e.message}")
  error("Aborting!")
  exit(1)
end

def main(arg)
  input = if File.readable?(arg)
            File.open(arg)
          else
            STDIN
          end
  puts(eval(input))
end

main(ARGV[0].to_s)
