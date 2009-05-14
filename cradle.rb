# A compiler skeleton, or cradle, as described by Jack Crenshaw in his
# famous book "Let's Build a Compiler".  At least in the beginning,
# this code will closely reflect the Pascal code written by Jack.
# Over time it may become more idiomatic, however this is an academic
# exercise.

class ParseError < StandardError; end 

class Compiler
  def initialize(input=STDIN)
    @look = ''                   # next lookahead char
    @input = input               # stream to read from

    # seed the lexer
    get_char
  end

  # Read the next character from the input stream
  def get_char
    @look = @input.getc
    @look = @look.chr if @look
  end

  # Report error and halt
  def abort(msg)
    raise ParseError, msg
  end

  # Report what was expected
  def expected(what)
    if eof?
      raise ParseError, "Premature end of file, expected: #{what}."
    else
      raise ParseError, "Expected: #{what}, got: #{@look}."
    end
  end
  
  # Match a specific input character
  def match(char)
    if @look == char
      get_char
    else
      expected("'#{char}'")
    end
  end

  # Recognize an alphabetical character
  def is_alpha(char)
    ('A'..'Z') === char.upcase
  end

  # Recognize a decimal digit
  def is_digit(char)
    ('0'..'9') === char
  end

  # Get an identifier
  def get_name
    expected('identifier') unless is_alpha(@look)
    c = @look
    get_char
    return c
  end

  # Get a number
  def get_num
    expected('integer') unless is_digit(@look)
    c = @look
    get_char
    return c
  end

  # Print a tab followed by a string and a newline
  def emit(s)
    puts "\t#{s}"
  end

  # Parse and translate a single mathematical term.  Result is in eax.
  def term
    emit("mov eax, #{get_num}")
  end

  # Parse an addition operator and the 2nd term.  The 1st term is
  # expected in ebx, and is added to the 2nd term leaving the result
  # in eax.
  def add
    match('+')
    term                        # result in eax
    emit("add eax, ebx")
  end

  # Parse a subtraction operator and the 2nd term (b).  The 1st term
  # (a) is expected in ebx, and the b is subtracted from a
  # leaving the result in eax.
  def subtract
    match('-')
    term                        # result in eax (b)
    emit("sub eax, ebx")        # subtract a from b (this is backwards)
    emit("neg eax")             # fix things up.  -(b-a) == a-b
  end

  # Parse and translate a mathematical expression of terms.  Result is
  # in eax.
  def expression
    term                        # result is in eax
    emit("mov ebx, eax")        # move 1st term to ebx (expected by
                                # add & subtract)
    case @look
    when '+': add
    when '-': subtract
    else
      expected('Addition or subtraction operator (+ or -)')
    end
  end

  def eof?
    @input.eof? && @look.nil?
  end
end
