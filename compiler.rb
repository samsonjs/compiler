# A compiler as described by Jack Crenshaw in his famous book "Let's
# Build a Compiler".  At least in the beginning, this code will
# closely reflect the Pascal code written by Jack.  Over time it may
# become more idiomatic, however this is an academic exercise.
#
# sjs
# may 2009

class ParseError < StandardError; end 

class Compiler
  def initialize(input=STDIN, output=STDOUT)
    @look = ''                   # next lookahead char
    @input = input               # stream to read from
    @output = output             # stream to write to

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
    @output.puts("\t#{s}")
  end

  # Parse and translate a single factor.  Result is in eax.
  def factor
    emit("mov eax, #{get_num}")
  end

  # Parse and translate a single term.  Result is in eax.
  def term
    factor                      # Result in eax.
    while ['*', '/'].include?(@look)
      # Stash the 1st factor on the stack.  This is expected by
      # multiply & divide.  Because they leave their results in eax
      # associativity works.  Each interim result is pushed on the
      # stack here.
      emit("push eax")

      case @look
      when '*': multiply
      when '/': divide
      else
        expected('Multiplication or division operator (* or /)')
      end
      emit("add esp, 4")        # Remove the 1st factor from the stack.
    end
  end

  # Parse and translate a mathematical expression of terms.  Result is
  # in eax.
  def expression
    term                        # Result is in eax.

    while ['+', '-'].include?(@look)
      # Stash the 1st term on the stack.  This is expected by add &
      # subtract.  Because they leave their results in eax
      # associativity works.  Each interim result is pushed on the
      # stack here.
      emit("push eax")

      case @look
      when '+': add
      when '-': subtract
      else
        expected('Addition or subtraction operator (+ or -)')
      end
      emit("add esp, 4")        # Remove 1st term (a) from the stack.
    end
  end

  # Parse an addition operator and the 2nd term (b).  The result is
  # left in eax.  The 1st term (a) is expected on the stack.
  def add
    match('+')
    term                        # Result is in eax.
    emit("add eax, [esp]")      # Add a to b.
  end

  # Parse a subtraction operator and the 2nd term (b).  The result is
  # left in eax.  The 1st term (a) is expected on the stack.
  def subtract
    match('-')
    term                      # Result is in eax.
    emit("sub eax, [esp]")    # Subtract a from b (this is backwards).
    emit("neg eax")           # Fix things up.  -(b-a) == a-b
  end

  # Parse an addition operator and the 2nd term (b).  The result is
  # left in eax.  The 1st term (a) is expected on the stack.
  def multiply
    match('*')
    factor                      # Result is in eax.
    emit("imul dword [esp]")    # Multiply a by b.
  end

  # Parse a division operator and the divisor (b).  The result is
  # left in eax.  The dividend (a) is expected on the stack.
  def divide
    match('/')
    factor                      # Result is in eax.
    emit("xchg eax, [esp]")     # Swap the divisor and dividend into
                                # the correct places.
    emit("idiv dword [esp]")    # Divide a (eax) by b ([esp]).
  end

  def eof?
    @input.eof? && @look.nil?
  end
end
