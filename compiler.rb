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

    # reserved words (... constant?)
    #
    # if, else, end, while, until, repeat, do, for, break, true, false, print,
    # not, and, or, add, subtract, multiply, divide, xor, bool tests
    @keywords = %w[i l e w u r f d b t f p ! & | + - * / ^ = < > #]

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
      # TODO arg list
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
      boolean_expression
      match(')')
    elsif alpha?(@look)
      identifier                # or call
    elsif digit?(@look)
      x86_mov(:eax, get_number)
    else
      expected(:'integer, identifier, function call, or parenthesized expression')
    end
  end

  # Parse a signed factor.
  def signed_factor
    sign = @look
    match(sign) if sign == '-' || sign == '+'
    factor
    x86_neg(:eax) if sign == '-'
  end

  # Parse and translate a single term (factor or mulop).  Result is in
  # eax.
  def term
    signed_factor                      # Result in eax.

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
    term                      # Result is in eax.

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
    signed_factor               # Result is in eax.
    x86_imul('dword [esp]')     # Multiply a by b.
  end

  # Parse a division operator and the divisor (b).  The result is
  # left in eax.  The dividend (a) is expected on the stack.
  def divide
    match('/')
    signed_factor               # Result is in eax.
    x86_xchg(:eax, '[esp]')     # Swap the divisor and dividend into
                                # the correct places.

    # idiv uses edx:eax as the dividend so we need to ensure that edx
    # is correctly sign-extended w.r.t. eax.
    emit('cdq')       # Sign-extend eax into edx (Convert Double to
                      # Quad).
    x86_idiv('dword [esp]')     # Divide a (eax) by b ([esp]).
  end


  #######################
  # boolean expressions #
  #######################

  def boolean_expression
    boolean_term

    while orop?
      x86_push(:eax)
      case @look
      when '|': or_expr
      when '^': xor_expr
      end
      x86_add(:esp, 4)
    end
  end

  def or_expr
    match('|')
    boolean_term
    x86_or(:eax, '[esp]')
  end

  def xor_expr
    match('^')
    boolean_term
    x86_xor(:eax, '[esp]')
  end

  def boolean_term
    not_factor

    while andop?
      x86_push(:eax)
      # and_expr
      match('&')
      not_factor
      x86_and(:eax, '[esp]')
      x86_add(:esp, 4)
    end
  end

  def boolean_factor
    if boolean?(@look)
      if get_boolean
        x86_mov(:eax, -1)
      else
        x86_xor(:eax, :eax)
      end
    else
      relation
    end
  end

  def not_factor
    if @look == '!'
      match('!')
      boolean_factor
      make_boolean(:eax)        # ensure it is -1 or 0...
      x86_not(:eax)             # so that not is also boolean not
    else
      boolean_factor
    end
  end

  # Convert any identifier to a boolean (-1 or 0).  This is
  # semantically equivalent to !!reg in C or Ruby.
  def make_boolean(reg=:eax)
    end_label = unique_label(:endmakebool)
    x86_cmp(reg, 0)             # if false do nothing
    x86_jz(end_label)
    x86_mov(reg, -1)            # truthy, make it true
    emit_label(end_label)
  end

  def get_boolean
    expected(:boolean) unless boolean?(@look)
    value = @look == 't'
    get_char
    value
  end

  def relation
    expression
    if relop?
      x86_push(:eax)
      case @look
      when '=': eq_relation
      when '#': neq_relation
      when '>': gt_relation
      when '<': lt_relation
      # TODO ge, le (needs real tokens)
      end
    end
  end

  def eq_relation
    match('=')
    expression
    x86_pop(:ebx)
    x86_sub(:eax, :ebx)
    make_boolean
    x86_not(:eax)
  end

  def neq_relation
    match('#')
    expression
    x86_pop(:ebx)
    x86_sub(:eax, :ebx)
    make_boolean
  end

  def gt_relation
    match('>')
    gt_label = unique_label(:gt)
    end_label = unique_label(:endgt)
    expression
    x86_pop(:ebx)
    x86_cmp(:eax, :ebx)         # b - a < 0 if a > b
    x86_jl(gt_label)
    x86_xor(:eax, :eax)
    x86_jmp(end_label)
    emit_label(gt_label)
    x86_xor(:eax, :eax)
    x86_not(:eax)
    emit_label(end_label)
  end

  def lt_relation
    match('<')
    lt_label = unique_label(:lt)
    end_label = unique_label(:endlt)
    expression
    x86_pop(:ebx)
    x86_cmp(:ebx, :eax)         # a - b < 0 if a < b
    x86_jl(lt_label)
    x86_xor(:eax, :eax)
    x86_jmp(end_label)
    emit_label(lt_label)
    x86_xor(:eax, :eax)
    x86_not(:eax)
    emit_label(end_label)
  end


  ######################################
  # statements and controls structures #
  ######################################

  # Parse an assignment statement.  Value is in eax.
  def assignment
    name = get_name
    match('=')
    boolean_expression
    defvar(name) unless var?(name)
    x86_mov("dword [#{name}]", :eax)
  end

  # Parse a code block.
  def block(label=nil)
    until @look == 'l' || @look == 'e' || eof?
      case @look
      when 'i'
        if_else_stmt(label)
      when 'w'
        while_stmt
      when 'u'
        until_stmt
      when 'r'
        repeat_stmt
      when 'f'
        for_stmt
      when 'd'
        do_stmt
      when 'b'
        break_stmt(label)
      when 'p'
        print_stmt
        newline
      else
        assignment
        newline
      end
      skip_any_whitespace
    end
  end
  
  # Parse an if-else statement.
  def if_else_stmt(label)
    match('i')
    else_label = unique_label(:end_or_else)
    end_label = else_label      # only generated if else clause present
    condition
    skip_any_whitespace
    x86_jz(else_label)
    block(label)
    if @look == 'l'
      match('l')
      skip_any_whitespace
      end_label = unique_label(:endif) # now we need the 2nd label
      x86_jmp(end_label)
      emit_label(else_label)
      block(label)
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

  # s = 0
  # f x = 1 >> 5
  #   s = s + x
  # e
  def for_stmt
    match('f')
    start_label = unique_label(:for)
    end_label = unique_label(:endfor)
    counter = "[#{get_name}]"
    match('=')
    boolean_expression          # initial value
    x86_sub(:eax, 1)            # pre-decrement because of the
                                # following pre-increment
    x86_mov(counter, :eax)      # stash the counter in memory
    match('.'); match('.')
    boolean_expression          # final value
    skip_any_whitespace
    x86_push(:eax)              # stash final value on stack
    final = '[esp]'
    emit_label(start_label)
    x86_mov(:ecx, counter)      # get the counter
    x86_add(:ecx, 1)            # increment
    x86_mov(counter, :ecx)      # store the counter
    x86_cmp(final, :ecx)        # check if we're done
    x86_jz(end_label)           # if so jump to the end
    block(end_label)            # otherwise execute the block
    match('e')
    x86_jmp(start_label)        # lather, rinse, repeat
    emit_label(end_label)
    x86_add(:esp, 4)            # clean up the stack
  end

  # d 5
  #   ...
  # e
  def do_stmt
    match('d')
    start_label = unique_label(:do)
    end_label = unique_label(:enddo)
    boolean_expression
    skip_any_whitespace
    x86_mov(:ecx, :eax)
    x86_push(:ecx)
    counter = '[esp]'
    emit_label(start_label)
    x86_mov(counter, :ecx)
    block(end_label)
    x86_mov(:ecx, counter)
    match('e')
    x86_loop(start_label)
    x86_sub(:esp, 4)
    emit_label(end_label)
    x86_add(:esp, 4)
  end

  def break_stmt(label)
    match('b')
    if label
      x86_jmp(label)
    else
      expected(:'break to be somewhere useful',
               :got => :'a break outside a loop')
    end
  end

  # Evaluates any expression for now.  There are no boolean operators.
  def condition
    boolean_expression
    skip_whitespace
    x86_cmp(:eax, 0)            # 0 is false, anything else is true
  end

  def print_stmt
    match('p')
    # define a lookup table of digits
    unless var?('DIGITS')
      defvar('DIGITS', 4)
      x86_mov('dword [DIGITS]',    0x33323130)
      x86_mov('dword [DIGITS+4]',  0x37363534)
      x86_mov('dword [DIGITS+8]',  0x62613938)
      x86_mov('dword [DIGITS+12]', 0x66656463)
    end
    # 3 dwords == 12 chars
    defvar('HEX', 3) unless var?('HEX')
    # TODO check sign and prepend '-' if negative
    x86_mov('word [HEX]', 0x7830) # "0x" == [48, 120]
    x86_mov('word [HEX+10]', 0xa)  # newline + null terminator
    boolean_expression
    # convert eax to a hex string
    x86_lea(:esi, '[DIGITS]')
    x86_lea(:edi, '[HEX+9]')
    # build the string backwards (right to left), byte by byte
    x86_mov(:ecx, 4)
    emit_label(loop_label=unique_label)
    # low nybble of nth byte
    x86_movzx(:ebx, :al)
    x86_and(:bl, 0x0f)        # isolate low nybble
    x86_movzx(:edx, 'byte [esi+ebx]')
    x86_mov('byte [edi]', :dl)
    x86_dec(:edi)
    # high nybble of nth byte
    x86_movzx(:ebx, :al)
    x86_and(:bl, 0xf0)        # isolate high nybble
    x86_shr(:bl, 4)
    x86_mov(:dl, 'byte [esi+ebx]')
    x86_mov('byte [edi]', :dl)
    x86_dec(:edi)
    x86_shr(:eax, 8)
    x86_loop(loop_label)
    x86_mov(:eax, 4)            # SYS_write
    x86_mov(:ebx, 1)            # STDOUT
    x86_lea(:ecx, '[HEX]')
    x86_mov(:edx, 11)           # excluding term, max # of chars to print
    x86_int(0x80)
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

  def relop?
    @look == '=' || @look == '#' || @look == '<' || @look == '>'
  end

  def orop?
    @look == '|' || @look == '^'
  end

  def andop?
    @look == '&'
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

  def boolean?(char)
    char == 't' || char == 'f'
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


  # Parse a name (identifier).
  def get_name
    expected(:identifier) unless alpha?(@look)
    name = many(method(:alnum?))
    if @keywords.include?(name)
      expected(:identifier, :got => :keyword)
    end
    name
  end

  # Parse a number.
  def get_number
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
  def defvar(name, dwords=1)
    unless var?(name)
      @bss << "#{name}: resd #{dwords}\n"
      @vars[name] = name
    else
      STDERR.puts "[warning] attempted to redefine #{name}"
    end
  end

  def var?(name)
    @vars[name]
  end

  def var(name)
    @vars[name]
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
    emit("mov #{dest}, #{src.is_a?(Numeric) ? "0x#{src.to_s(16)}" : src}")
  end

  def x86_movzx(dest, src)
    emit("movzx #{dest}, #{src}")
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

  def x86_inc(op)
    emit("inc #{op}")
  end

  def x86_dec(op)
    emit("dec #{op}")
  end

  def x86_push(reg)
    emit("push #{reg}")
  end

  def x86_pop(reg)
    emit("pop #{reg}")
  end

  def x86_call(label)
    emit("call #{label}")
  end

  def x86_neg(reg)
    emit("neg #{reg}")
  end

  def x86_not(rm32)
    emit("not #{rm32}")
  end

  def x86_xchg(op1, op2)
    emit("xchg #{op1}, #{op2}")
  end

  def x86_and(op1, op2)
    emit("and #{op1}, #{op2}")
  end

  def x86_or(op1, op2)
    emit("or #{op1}, #{op2}")
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

  def x86_jl(label)
    emit("jl #{label}")
  end

  def x86_cmp(a, b)
    emit("cmp #{a}, #{b}")
  end

  def x86_lea(a, b)
    emit("lea #{a}, #{b}")
  end

  def x86_shr(a, b)
    emit("shr #{a}, #{b}")
  end

  def x86_loop(label)
    emit("loop #{label}")
  end

  def x86_int(num)
    emit("int 0x#{num.to_s(16)}")
  end
end
