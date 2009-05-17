# A compiler as described by Jack Crenshaw in his famous book "Let's
# Build a Compiler".  At least in the beginning, this code will
# closely reflect the Pascal code written by Jack.  Over time it may
# become more idiomatic, however this is an academic exercise.
#
# sjs
# may 2009

class ParseError < StandardError
  attr_reader :caller, :context
  def initialize(caller, context=nil)
    @caller = caller
    @context = context
  end
end

class Compiler
  attr_reader :data, :bss, :code
  
  def initialize(input=STDIN)
    @look = ''                   # next lookahead char
    @input = input               # stream to read from
    @data = ''                   # data section
    @bss = ''                    # bss section
    @code = ''                   # code section
    @vars = {}                   # symbol table
    @num_labels = 0              # used to generate unique labels
    @num_labels_with_suffix = Hash.new(0)
    @num_conditions = 0
    @break_stack = []            # for breaking out of loops

    # reserved words (... constant?)
    #
    # if, else, end, while, until, repeat, break
    @keywords = %w[i l e w u r b]

    # seed the lexer
    get_char
  end

  def parse
    block
    expected(:'end of file') unless eof?
    [@data, @bss, @code]
  end


  # Parse and translate an identifier or function call.
  def identifier
    name = get_name

    if @look == '('
      # function call
      match('(')
      match(')')
      x86_call(name)
    else
      # variable access
      x86_mov(:eax, "dword [#{name}]")
    end
  end

  # Parse and translate a single factor.  Result is in eax.
  def factor
    if @look == '('
      match('(')
      expression
      match(')')
    elsif alpha?(@look)
      identifier
    elsif digit?(@look)
      x86_mov(:eax, get_num)
    else
      expected(:'integer, identifier, or parenthesized expression')
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
      x86_push(:eax)

      if @look == '*'
        multiply
      else
        divide
      end

      x86_add(:esp, 4)        # Remove the 1st factor from the stack.
    end
  end

  # Parse and translate a general expression of terms.  Result is
  # in eax.
  def expression
    if addop?
      # Clear eax simulating a zero before unary plus and minus
      # operations.
      x86_xor(:eax, :eax)
    else
      term                      # Result is in eax.
    end

    while addop?
      # Stash the 1st term on the stack.  This is expected by add &
      # subtract.  Because they leave their results in eax
      # associativity works.  Each interim result is pushed on the
      # stack here.
      x86_push(:eax)

      if @look == '+'
        add
      else
        subtract
      end

      x86_add(:esp, 4)        # Remove 1st term (a) from the stack.
    end
  end

  # Parse an assignment statement.  Value is in eax.
  def assignment
    name = get_name
    match('=')
    expression
    var(name)
    x86_mov("dword [#{name}]", :eax)
  end

  # Parse a statement.
  def statement
    case @look
    when 'i'
      if_else_stmt
    when 'w'
      while_stmt
    when 'u'
      until_stmt
    when 'r'
      repeat_stmt
    when 'b'
      break_stmt
    else
      assignment
      newline
    end
  end

  # Parse a code block.
  def block(label=nil)
    @break_stack.push(label) if label
    until @look == 'l' || @look == 'e' || eof?
      statement
      skip_any_whitespace
    end
    @break_stack.pop if label
  end
  
  # Parse an if-else statement.
  def if_else_stmt
    match('i')
    condition
    skip_any_whitespace
    else_label = unique_label(:end_or_else)
    end_label = else_label      # only generated if else clause present
    x86_jz(else_label)
    block
    if @look == 'l'
      match('l')
      skip_any_whitespace
      end_label = unique_label(:endif) # now we need the 2nd label
      x86_jmp(end_label)
      emit_label(else_label)
      block
    end
    match('e')
    emit_label(end_label)
  end

  def while_stmt
    match('w')
    while_label = unique_label(:while)
    end_label = unique_label(:endwhile)
    emit_label(while_label)
    condition
    skip_any_whitespace
    x86_jz(end_label)
    block(end_label)
    match('e')
    x86_jmp(while_label)
    emit_label(end_label)
  end

  def until_stmt
    match('u')
    until_label = unique_label(:until)
    end_label = unique_label(:enduntil)
    emit_label(until_label)
    condition
    skip_any_whitespace
    x86_jnz(end_label)
    block(end_label)
    match('e')
    x86_jmp(until_label)
    emit_label(end_label)
  end

  def repeat_stmt
    match('r')
    skip_any_whitespace         # no condition, slurp whitespace
    repeat_label = unique_label(:repeat)
    end_label = unique_label(:endrepeat)
    emit_label(repeat_label)
    block(end_label)
    match('e')
    x86_jmp(repeat_label)
    emit_label(end_label)
  end

  def break_stmt
    match('b')
    if @break_stack.empty?
      expected(:'break to be somewhere useful',
               :got => :'a break without a loop')
    end
    x86_jmp(@break_stack.last)
  end

  # Evaluates any expression for now.  There are no boolean operators.
  def condition
    # @num_conditions += 1
    # emit("<condition ##{@num_conditions}>")
    expression
    x86_cmp(:eax, 0)            # 0 is false, anything else is true
    skip_whitespace
  end

  # Parse an addition operator and the 2nd term (b).  The result is
  # left in eax.  The 1st term (a) is expected on the stack.
  def add
    match('+')
    term                        # Result is in eax.
    x86_add(:eax, '[esp]')         # Add a to b.
  end

  # Parse a subtraction operator and the 2nd term (b).  The result is
  # left in eax.  The 1st term (a) is expected on the stack.
  def subtract
    match('-')
    term                      # Result, b, is in eax.
    x86_neg(:eax)             # Fake the subtraction.  a - b == a + -b
    x86_add(:eax, '[esp]')    # Add a and -b.
  end

  # Parse an addition operator and the 2nd term (b).  The result is
  # left in eax.  The 1st term (a) is expected on the stack.
  def multiply
    match('*')
    factor                      # Result is in eax.
    x86_imul('dword [esp]')     # Multiply a by b.
  end

  # Parse a division operator and the divisor (b).  The result is
  # left in eax.  The dividend (a) is expected on the stack.
  def divide
    match('/')
    factor                      # Result is in eax.
    x86_xchg(:eax, '[esp]')     # Swap the divisor and dividend into
                                # the correct places.

    # idiv uses edx:eax as the dividend so we need to ensure that edx
    # is correctly sign-extended w.r.t. eax.
    emit('cdq')       # Sign-extend eax into edx (Convert Double to
                      # Quad).
    x86_idiv('dword [esp]')     # Divide a (eax) by b ([esp]).
  end



############
# internal #
############


  def eof?
    @input.eof? && @look.nil?
  end

  def addop?
    @look == '+' || @look == '-'
  end

  def mulop?
    @look == '*' || @look == '/'
  end


  # Read the next character from the input stream.
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
  def expected(what, options={})
    got = options.has_key?(:got) ? options[:got] : @look
    got, what = *[got, what].map {|x| x.is_a?(Symbol) ? x : "'#{x}'" }
    if eof?
      raise ParseError.new(caller), "Premature end of file, expected: #{what}."
    else
      context = (@input.readline rescue '(EOF)').gsub("\n", "\\n")
      raise ParseError.new(caller, context), "Expected #{what} but got #{got}."
    end
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
    char == ' ' || char == "\t"
  end

  def any_whitespace?(char)
    char == ' ' || char == "\t" || char == "\n" || char == "\r"
  end

  # Parse one or more newlines.
  def newline
    if @look == "\n" || @look == "\r"
      get_char while @look == "\n" || @look == "\r"
    else
      expected(:newline)
    end
  end

  # Match a specific input character.
  def match(char)
    expected(char) unless @look == char
    get_char
    skip_whitespace
  end

  # Parse zero or more consecutive characters for which the test is
  # true.
  def many(test)
    token = ''
    while test[@look]
      token << @look
      get_char
    end
    skip_whitespace
    token
  end


  # Get an identifier.
  def get_name
    expected(:identifier) unless alpha?(@look)
    name = many(method(:alnum?))
    if @keywords.include?(name)
      expected(:identifier, :got => :keyword)
    end
    name
  end

  # Get a number.
  def get_num
    expected(:integer) unless digit?(@look)
    many(method(:digit?))
  end

  # Skip leading whitespace.
  def skip_whitespace
    get_char while whitespace?(@look)
  end

  # Skip leading whitespace including newlines.
  def skip_any_whitespace
    get_char while any_whitespace?(@look)
  end


  # Define a constant in the .data section.
  def equ(name, value)
    @data << "#{name}\tequ  #{value}"
  end

  # Define a variable with the given name and size (in dwords).
  def var(name, dwords=1)
    unless @vars[name]
      @bss << "#{name}: resd #{dwords}\n"
      @vars[name] = name
    # else
    #   raise ParseError, "identifier #{name} redefined"
    end
  end

  # Emit a line of code wrapped between a tab and a newline.
  def emit(code, options={})
    tab = options.has_key?(:tab) ? options[:tab] : "\t"
    @code << "#{tab}#{code}\n"
  end

  def emit_label(name=unique_label)
    emit("#{name}:", :tab => nil)
  end

  # Generate a unique label.
  def unique_label(suffix=nil)
    @num_labels += 1
    if suffix
      @num_labels_with_suffix[suffix] += 1
      suffix = "_#{suffix}_#{@num_labels_with_suffix[suffix]}"
    end
    "L#{sprintf "%06d", @num_labels}#{suffix}"
  end


  # Some asm methods for convenience and arity checks.

  def x86_mov(dest, src)
    emit("mov #{dest}, #{src}")
  end

  def x86_add(dest, src)
    emit("add #{dest}, #{src}")
  end

  def x86_sub(dest, src)
    emit("sub #{dest}, #{src}")
  end

  def x86_imul(op)
    emit("imul #{op}")
  end

  def x86_idiv(op)
    emit("idiv #{op}")
  end

  def x86_push(reg)
    emit("push #{reg}")
  end

  def x86_call(label)
    emit("call #{label}")
  end

  def x86_neg(reg)
    emit("neg #{reg}")
  end

  def x86_xchg(op1, op2)
    emit("xchg #{op1}, #{op2}")
  end

  def x86_xor(op1, op2)
    emit("xor #{op1}, #{op2}")
  end

  def x86_jz(label)
    emit("jz #{label}")
  end

  def x86_jnz(label)
    emit("jnz #{label}")
  end

  def x86_jmp(label)
    emit("jmp #{label}")
  end

  def x86_cmp(a, b)
    emit("cmp #{a}, #{b}")
  end
end
