require "interpreter"
require "stringio"

def error(msg)
  warn(msg)
end

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
    $stdin
  end
  # Evaluating what was typed in is the whole point of this script.
  puts(eval(input)) # standard:disable Security/Eval
end

main(ARGV[0].to_s)
