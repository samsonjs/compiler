# A compiler as described by Jack Crenshaw in his famous book "Let's
# Build a Compiler".  At least in the beginning, this code will
# closely reflect the Pascal code written by Jack.  Over time it may
# become more idiomatic, however this is an academic exercise.
#
# sjs
# may 2009

class ParseError < StandardError; end 

class Compiler
  attr_reader :data, :bss, :code
  
  def initialize(input=STDIN)
    @look = ''                   # next lookahead char
    @input = input               # stream to read from
    @data = ''                   # data section
    @bss = ''                    # bss section
    @code = ''                   # code section

    # seed the lexer
    get_char
  end

  def parse
    statement until eof?
    [@data, @bss, @code]
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
    if eof?
      raise ParseError, "Premature end of file, expected: #{what}."
    else
      raise ParseError, "Expected: #{what}, got: #{@look} (##{@look[0]})."
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
  def alpha?(char)
    ('A'..'Z') === char.upcase
  end

  # Recognize a decimal digit
  def digit?(char)
    ('0'..'9') === char
  end

  # Get an identifier
  def get_name
    expected('identifier') unless alpha?(@look)
    c = @look
    get_char
    return c
  end

  # Get a number
  def get_num
    expected('integer') unless digit?(@look)
    c = @look
    get_char
    return c
  end

  # Define a constant in the .data section.
  def equ(name, value)
    @data << "#{name}\tequ  #{value}"
  end

  # Define a variable with the given name and size (in dwords).
  def var(name, dwords=1)
    @bss << "#{name}: resd #{dwords}\n"
  end

  # Emit a line of code wrapped between a tab and a newline.
  def emit(s)
    @code << "\t#{s}\n"
  end

  # Parse and translate an identifier or function call.
  def identifier
    name = get_name

    if @look == '('
      # function call
      match('(')
      match(')')
      call(name)
    else
      # variable access
      mov("eax", "dword [#{name}]")
    end
  end

  # Parse and translate a single factor.  Result is in eax.
  def factor
    case 
    when @look == '('
      match('(')
      expression
      match(')')
    when alpha?(@look)
      identifier
    when digit?(@look)
      mov("eax", get_num)
    else
      expected("a number, identifier, or an expression wrapped in parens")
    end
  end

  # Parse and translate a single term (factor or mulop).  Result is in
  # eax.
  def term
    factor                      # Result in eax.
    while mulop?
      # Stash the 1st factor on the stack.  This is expected by
      # multiply & divide.  Because they leave their results in eax
      # associativity works.  Each interim result is pushed on the
      # stack here.
      push("eax")

      if @look == '*'
        multiply
      else
        divide
      end

      add("esp", 4)        # Remove the 1st factor from the stack.
    end
  end

  # Parse and translate a general expression of terms.  Result is
  # in eax.
  def expression
    if addop?
      # Clear eax simulating a zero before unary plus and minus
      # operations.
      xor("eax", "eax")
    else
      term                      # Result is in eax.
    end

    while addop?
      # Stash the 1st term on the stack.  This is expected by add &
      # subtract.  Because they leave their results in eax
      # associativity works.  Each interim result is pushed on the
      # stack here.
      push("eax")

      if @look == '+'
        add
      else
        subtract
      end

      add("esp", 4)        # Remove 1st term (a) from the stack.
    end
  end

  # Parse an assignment statement.  Value is in eax.
  def assignment
    name = get_name
    match('=')
    expression
    var(name)
    mov("dword [#{name}]", "eax")
  end

  # Parse one or more newlines.
  def newline
    if @look == "\n" || @look == "\r"
      get_char while @look == "\n" || @look == "\r"
    else
      expected('newline')
    end
  end

  # Parse an assignment expression followed by a newline.
  def statement
    assignment
    newline
  end


  # Parse an addition operator and the 2nd term (b).  The result is
  # left in eax.  The 1st term (a) is expected on the stack.
  def add
    match('+')
    term                        # Result is in eax.
    add('eax', '[esp]')         # Add a to b.
  end

  # Parse a subtraction operator and the 2nd term (b).  The result is
  # left in eax.  The 1st term (a) is expected on the stack.
  def subtract
    match('-')
    term                      # Result is in eax.
    sub('eax', '[esp]')       # Subtract a from b (this is backwards).
    neg('eax')                # Fix things up.  -(b-a) == a-b
  end

  # Parse an addition operator and the 2nd term (b).  The result is
  # left in eax.  The 1st term (a) is expected on the stack.
  def multiply
    match('*')
    factor                      # Result is in eax.
    imul('dword [esp]')         # Multiply a by b.
  end

  # Parse a division operator and the divisor (b).  The result is
  # left in eax.  The dividend (a) is expected on the stack.
  def divide
    match('/')
    factor                      # Result is in eax.
    xchg('eax', '[esp]')        # Swap the divisor and dividend into
                                # the correct places.
    idiv('dword [esp]')         # Divide a (eax) by b ([esp]).
  end



#######
private
#######

  def eof?
    @input.eof? && @look.nil?
  end

  def addop?
    @look == '+' || @look == '-'
  end

  def mulop?
    @look == '*' || @look == '/'
  end


  # Some asm methods for convenience and arity checks.

  def mov(dest, src)
    emit("mov #{dest}, #{src}")
  end

  def add(dest, src)
    emit("add #{dest}, #{src}")
  end

  def sub(dest, src)
    emit("sub #{dest}, #{src}")
  end

  def imul(op)
    emit("imul #{op}")
  end

  def idiv(op)
    emit("idiv #{op}")
  end

  def push(reg)
    emit("push #{reg}")
  end

  def call(label)
    emit("call #{label}")
  end

  def neg(reg)
    emit("neg #{reg}")
  end

  def xchg(op1, op2)
    emit("xchg #{op1}, #{op2}")
  end
end
