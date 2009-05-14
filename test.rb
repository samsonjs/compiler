require 'compiler'
require 'stringio'

MaxRetries = 1

def error(msg) STDERR.puts(msg) end

# Main program
def main 
  retries = 0
  input = StringIO.new(ARGV[0] || '5-5')
  output = StringIO.new
  parse = Compiler.new(input, output)
  until parse.eof?
    begin
      parse.expression
    rescue ParseError => e
      error("[error] #{e.message}")
      if retries < MaxRetries
        retries += 1
        error("Skipping token...")
        parse.get_char
        retry
      else
        error("Aborting!")
        break
      end
    end
  end
  output.string
end

code = main
File.open("test.asm", "w") do |f|
  f.puts(File.readlines("prologue.asm"))
  f.puts(code)
  f.puts(File.readlines("epilogue.asm"))
end

