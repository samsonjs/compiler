# An interpreter as described by Jack Crenshaw in his famous book
# "Let's Build a Compiler".  At least in the beginning, this code will
# closely reflect the Pascal code written by Jack.  Over time it may
# become more idiomatic, however this is an academic exercise.
#
# sjs
# may 2009

class ParseError < StandardError; end 

class Interpreter
  def initialize(input=STDIN)
    @look = ''                   # next lookahead char
    @input = input               # stream to read from
    @vars = Hash.new(0)

    # seed the lexer
    get_char
  end

  def run
    statement until eof? || @look == '.'
  end

  # Read the next character from the input stream
  def get_char
    @look = if @input.eof?
              nil
            else
              @input.readbyte.chr
            end
  end

  # Report error and halt
  def abort(msg)
    raise ParseError, msg
  end

  # Report what was expected
  def expected(what)
    msg = if eof?
            "Premature end of file, expected: #{what}."
          else
            "Expected: #{what}, got: #{@look}."
          end
    abort(msg)
  end


  # Define a variable.
  def define(name, value=nil)
    abort("already defined '#{name}'") if @vars.has_key?(name)
    set(name, value)
  end

  # Set the value of a variable.
  def set(name, value)
    @vars[name] = value
  end

  # Retrieve the value of a variable.
  def get(name)
    abort("undefined variable '#{name}'") unless @vars.has_key?(name)
    @vars[name]
  end


  # Recognize an alphabetical character.
  def alpha?(char)
    ('A'..'Z') === char.upcase
  end

  # Recognize a decimal digit.
  def digit?(char)
    ('0'..'9') === char
  end

  # Recognize an alphanumeric character.
  def alnum?(char)
    alpha?(char) || digit?(char)
  end

  def whitespace?(char)
    char == ' ' || char == '\t'
  end


  # Match a specific input character.
  def match(char)
    expected("'#{char}'") unless @look == char
    get_char
    skip_whitespace
  end

  # Parse zero or more consecutive characters for which the test is
  # true.
  def many(test)
    token = ''
    while test.call(@look)
      token << @look
      get_char
    end
    skip_whitespace
    token
  end

  # Get an identifier.
  def get_name
    expected('identifier') unless alpha?(@look)
    many(method(:alpha?))
  end

  # Get a number.
  def get_num
    expected('integer') unless digit?(@look)
    many(method(:digit?)).to_i
  end

  # Skip all leading whitespace.
  def skip_whitespace
    get_char while whitespace?(@look)
  end



  # Parse and evaluate a factor.
  def factor
    if @look == '('
      match('(')
      value = expression
      match(')')
    elsif alpha?(@look)
      value = get(get_name)
    else
      value = get_num
    end
    value
  end

  # Parse and evaluate a term.
  def term
    value = factor
    while mulop?
      case @look
      when '*'
        match('*')
        value *= factor
      when '/'
        match('/')
        value /= factor
      end
    end
    value
  end

  # Parse and evaluate a mathematical expression.
  def expression
    # Fake unary plus and minus by prepending a 0 to them.
    value = if addop? then 0 else term end
    while addop?
      case @look
      when '+'
        match('+')
        value += term
      when '-'
        match('-')
        value -= term
      end
    end
    value
  end

  # Parse one or more newlines.
  def newline
    if @look == "\n" || @look == "\r"
      get_char while @look == "\n" || @look == "\r"
    else
      expected('newline')
    end
  end

  # Parse and evaluate an assignment statement.
  def assignment
    name = get_name
    match('=')
    set(name, expression)
  end

  # Parse and evaluate any kind of statement.
  def statement
    value = case @look
            when '?': input
            when '!': output
            else
              assignment
            end
    newline
    value
  end

  # Read and store a number from standard input.
  def input
    match('?')
    set(get_name, gets.to_i)
  end

  # Print a value to standard output.
  def output
    match('!')
    puts(get(get_name))
  end


private

  def eof?
    @input.eof? && @look.nil?
  end

  def addop?
    @look == '+' || @look == '-'
  end

  def mulop?
    @look == '*' || @look == '/'
  end
end
