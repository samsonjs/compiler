# A compiler as described by Jack Crenshaw in his famous book "Let's
# Build a Compiler".  At least in the beginning, this code will
# closely reflect the Pascal code written by Jack.  Over time it may
# become more idiomatic, however this is an academic exercise.
#
# sjs
# may 2009

# XXX Comment if unused, unroller is too fucking slow! rubygems is a bit
#     of a slouch too.
# 
# require 'rubygems'
# require 'unroller'

class ParseError < StandardError
  attr_reader :caller, :context
  def initialize(caller, context=nil)
    @caller = caller
    @context = context
  end
end

class Compiler

  Keywords = %w[
    if else end while until repeat for to do break
    print
  ]
  
  attr_reader :asm

  def initialize(input, asm)
    # XXX for development only!
    @indent = 0                  # for pretty printing

    @look = ''                   # Next lookahead char.
    @token = nil                 # Type of last read token.
    @value = nil                 # Value of last read token.
    @input = input               # Stream to read from.

    @asm = asm

    # seed the lexer
    get_char
  end

  def compile
    block
    expected(:'end of file') unless eof?
    asm.output
  end

  # Scan the input stream for the next token.
  def scan
    return if @look.nil?        # eof
    if alpha?(@look)
      get_name
    elsif digit?(@look)
      get_number
    elsif op_char?(@look)
      get_op
    elsif newline?(@look)
      skip_any_whitespace
      scan
    else
      # XXX default to single char op... should probably raise.
      @token = :op
      @value = @look
      get_char
    end
  end

  # Parse and translate an identifier or function call.
  def identifier
    name = get_name

    if @look == '('
      # function call
      match('(')
      # TODO arg list
      match(')')
      asm.call(name)
    else
      # variable access
      asm.mov(:eax, "dword [#{name}]")
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
      asm.mov(:eax, get_number.to_i)
    else
      expected(:'integer, identifier, function call, or parenthesized expression', :got => @look)
    end
  end

  # Parse a signed factor.
  def signed_factor
    sign = @look
    match(sign) if op?(:unary, sign)
    factor
    asm.neg(:eax) if sign == '-'
  end

  # Parse and translate a single term (factor or mulop).  Result is in
  # eax.
  def term
    signed_factor                      # Result in eax.

    while op?(:mul, @look)
      pushing(:eax) do
        case @look
        when '*': multiply
        when '/': divide
        end
      end
    end
  end

  # Parse and translate a general expression of terms.  Result is
  # in eax.
  def expression
    term                      # Result is in eax.

    while op_char?(@look, :add)
      pushing(:eax) do
        case @look
        when '+': add
        when '-': subtract
        end
      end
    end
  end

  # Parse an addition operator and the 2nd term (b).  The result is
  # left in eax.  The 1st term (a) is expected on the stack.
  def add
    match('+')
    term                        # Result is in eax.
    asm.add(:eax, '[esp]')         # Add a to b.
  end

  # Parse a subtraction operator and the 2nd term (b).  The result is
  # left in eax.  The 1st term (a) is expected on the stack.
  def subtract
    match('-')
    term                      # Result, b, is in eax.
    asm.neg(:eax)             # Fake the subtraction.  a - b == a + -b
    asm.add(:eax, '[esp]')    # Add a and -b.
  end

  # Parse an addition operator and the 2nd term (b).  The result is
  # left in eax.  The 1st term (a) is expected on the stack.
  def multiply
    match('*')
    signed_factor               # Result is in eax.
    asm.imul('dword [esp]')     # Multiply a by b.
  end

  # Parse a division operator and the divisor (b).  The result is
  # left in eax.  The dividend (a) is expected on the stack.
  def divide
    match('/')
    signed_factor               # Result is in eax.
    asm.xchg(:eax, '[esp]')     # Swap the divisor and dividend into
                                # the correct places.

    # idiv uses edx:eax as the dividend so we need to ensure that edx
    # is correctly sign-extended w.r.t. eax.
    asm.cdq              # Sign-extend eax into edx (Convert Double to
                         # Quad).
    asm.idiv('dword [esp]')     # Divide a (eax) by b ([esp]).
  end


  ###################
  # bit expressions #
  ###################

  def bitor_expr
    match('|')
    term
    asm.or(:eax, '[esp]')
  end

  def bitand_expr
    match('&')
    signed_factor
    asm.and_(:eax, '[esp]')
  end

  def xor_expr
    match('^')
    term
    asm.xor(:eax, '[esp]')
  end


  #######################
  # boolean expressions #
  #######################

  def boolean_expression
    boolean_term
    while @look == '|'
      op '||' do
        boolean_term
        # !!! this method has moved, IMPLEMENT THIS!
        emit("<logical or>")
      end
    end
  end

  def boolean_term
    not_factor
    while @look == '&'
      op '&&' do
        not_factor
        # !!! this method has moved, IMPLEMENT THIS!
        emit("<logical and>")
      end
    end
  end

  def boolean_factor
    if boolean?(@look)
      if get_boolean == 'true'
        asm.mov(:eax, -1)
      else
        asm.xor(:eax, :eax)
      end
      scan
    else
      relation
    end
  end

  def not_factor
    if @look == '!'
      match('!')
      boolean_factor
      make_boolean(:eax)        # ensure it is -1 or 0...
      asm.not(:eax)             # so that not is also boolean not
    else
      boolean_factor
    end
  end

  # Convert any identifier to a boolean (-1 or 0).  This is
  # semantically equivalent to !!reg in C or Ruby.
  def make_boolean(reg=:eax)
    end_label = asm.label(:endmakebool)
    asm.cmp(reg, 0)             # if false do nothing
    asm.jz(end_label)
    asm.mov(reg, -1)            # truthy, make it true
    asm.emit_label(end_label)
  end

  def relation
    expression
    if op_char?(@look, :rel)
      scan
      pushing(:eax) do
        case @value
        when '==': eq_relation
        when '!=': neq_relation
        when '>': gt_relation
        when '>=': ge_relation
        when '<': lt_relation
        when '<=': le_relation
        end
      end
    end
  end

  # a: [esp]
  # b: eax
  # 
  # If b - a is zero then a = b, and make_boolean will leave the zero
  # to effectively return false.  If b - a is non-zero then a != b,
  # and make_boolean will leave -1 (true) for us in eax.
  def neq_relation
    expression
    asm.sub(:eax, '[esp]')
    make_boolean
  end

  # Invert the != test for equal.
  def eq_relation
    neq_relation
    asm.not(:eax)
  end

  # > and < are both implemented in terms of jl (jump if less than).
  # We exploit the fact that cmp is the subtraction of src from dest
  # and order the terms appropriately for each function.  As for >=
  # and <=, they in turn are implemented in terms of > and <.  a is
  # greater than or equal to b if and only if a is *not* less than b.

  # The next 4 relations all compare 2 values a and b, then return
  # true (-1) if the difference was below zero and false (0)
  # otherwise (using JL, jump if less than).
  def cmp_relation(a, b, options={})
    # Invert the sense of the test?
    invert = options[:invert]

    true_label = asm.label(:cmp)
    end_label = asm.label(:endcmp)
    asm.cmp(a, b)
    asm.jl(true_label)

    asm.xor(:eax, :eax)         # return false
    asm.not(:eax) if invert     # (or true if inverted)
    asm.jmp(end_label)

    asm.emit_label(true_label)
    asm.xor(:eax, :eax)          # return true
    asm.not(:eax) unless invert  # (or false if inverted)

    asm.emit_label(end_label)
  end

  # a: [esp]
  # b: eax
  #
  # if a > b then b - a < 0
  def gt_relation
    expression
    cmp_relation(:eax, '[esp]') # b - a
  end

  # a: [esp]
  # b: eax
  #
  # if a < b then a - b < 0
  def lt_relation
    expression
    cmp_relation('[esp]', :eax) # a - b
  end

  # a: [esp]
  # b: eax
  #
  # if a >= b then !(a < b)
  def ge_relation
    expression
    # Compare them as in less than but invert the result.
    cmp_relation('[esp]', :eax, :invert => true)
  end

  # a: [esp]
  # b: eax
  # 
  # if a <= b then !(a > b)
  def le_relation
    expression
    # Compare them as in greater than but invert the result.
    cmp_relation(:eax, '[esp]', :invert => true)
  end


  ######################################
  # statements and controls structures #
  ######################################

  # Parse an assignment statement.  Value is in eax.
  def assignment
    name = @value
    match('=')
    boolean_expression
    asm.defvar(name) unless asm.var?(name)
    asm.mov("dword [#{name}]", :eax)
  end

  # Parse a code block.
  def block(label=nil)
    scan
    until @value == 'else' || @value == 'end' || eof?
      if @token == :keyword
        case @value
        when 'if'
          if_else_stmt(label)
        when 'while'
          while_stmt
        when 'until'
          until_stmt
        when 'repeat'
          repeat_stmt
        when 'for'
          for_stmt
        when 'do'
          do_stmt
        when 'break'
          break_stmt(label)
        when 'print'
          print_stmt
        else
          raise "unsupported keyword: #{@value}"
        end
      else
        assignment
      end
      scan
    end
  end
  
  # Parse an if-else statement.
  def if_else_stmt(label)
    else_label = asm.label(:end_or_else)
    end_label = else_label      # only generated if else clause
                                # present
    condition
    skip_any_whitespace
    asm.jz(else_label)
    @indent += 1
    block(label)
    @indent -= 1
    if @token == :keyword && @value == 'else'
      skip_any_whitespace
      end_label = asm.label(:endif) # now we need the 2nd label
      asm.jmp(end_label)
      asm.emit_label(else_label)
      @indent += 1
      block(label)
      @indent -= 1
    end
    match_word('end')
    asm.emit_label(end_label)
  end

  # Used to implement the Two-Label-Loops (while, until, repeat).
  # 
  # name:  Name of the loop for readable labels.
  # block: Code to execute at the start of each iteration. (e.g. a
  #        condition)
  def simple_loop(name)
    start_label = asm.label(:"loop_#{name}")
    end_label = asm.label(:"end_#{name}")
    asm.emit_label(start_label)

    yield(end_label)

    @indent += 1
    block(end_label)
    @indent -= 1
    match_word('end')
    asm.jmp(start_label)
    asm.emit_label(end_label)
  end

  def while_stmt
    simple_loop('while') do |end_label|
      condition
      skip_any_whitespace
      asm.jz(end_label)
    end
  end

  def until_stmt
    simple_loop('until') do |end_label|
      condition
      skip_any_whitespace
      asm.jnz(end_label)
    end
  end

  def repeat_stmt
    simple_loop('repeat') do |end_label|
      skip_any_whitespace
    end
  end

  # s = 0
  # f x = 1 to 5
  #   s = s + x
  # e
  def for_stmt
    counter = "[#{get_name}]"
    match('=')
    boolean_expression          # initial value
    asm.sub(:eax, 1)            # pre-decrement because of the
                                # following pre-increment
    asm.mov(counter, :eax)      # stash the counter in memory
    match_word('to', :scan => true)
    boolean_expression          # final value
    skip_any_whitespace
    asm.push(:eax)              # stash final value on stack
    final = '[esp]'

    simple_loop('for') do |end_label|
      asm.mov(:ecx, counter)      # get the counter
      asm.add(:ecx, 1)            # increment
      asm.mov(counter, :ecx)      # store the counter
      asm.cmp(final, :ecx)        # check if we're done
      asm.jz(end_label)           # if so jump to the end
    end

    asm.add(:esp, 4)            # clean up the stack
  end

  # do 5
  #   ...
  # end
  def do_stmt

    boolean_expression
    skip_any_whitespace
    asm.mov(:ecx, :eax)

    start_label = asm.label(:do)
    end_label = asm.label(:enddo)
    asm.emit_label(start_label)

    asm.push(:ecx)

    @indent += 1
    block(end_label)
    @indent -= 1

    asm.pop(:ecx)

    match_word('end')
    asm.loop_(start_label)

    # Phony push!  break needs to clean up the stack, but since we
    # don't know if there is a break at this point we fake a push and
    # always clean up the stack after.
    asm.sub(:esp, 4)

    asm.emit_label(end_label)

    # If there was a break we have to clean up the stack here.  If
    # there was no break we clean up the phony push above.
    asm.add(:esp, 4)
  end

  def break_stmt(label)
    if label
      asm.jmp(label)
    else
      expected(:'break to be somewhere useful',
               :got => :'a break outside a loop')
    end
  end

  # Evaluates any expression for now.  There are no boolean operators.
  def condition
    boolean_expression
    skip_whitespace
    asm.cmp(:eax, 0)            # 0 is false, anything else is true
  end

  # print eax in hex format
  def print_stmt
    asm.block do
      # define a lookup table of digits
      unless var?('DIGITS')
        defvar('DIGITS', 4)
        mov('dword [DIGITS]',    0x33323130)
        mov('dword [DIGITS+4]',  0x37363534)
        mov('dword [DIGITS+8]',  0x62613938)
        mov('dword [DIGITS+12]', 0x66656463)
      end
      # 3 dwords == 12 chars
      defvar('HEX', 3) unless var?('HEX')
      # TODO check sign and prepend '-' if negative
      mov('word [HEX]', 0x7830) # "0x" == [48, 120]
      mov('word [HEX+10]', 0xa)  # newline + null terminator
    end
    boolean_expression
    asm.block do
      # convert eax to a hex string
      lea(:esi, '[DIGITS]')
      lea(:edi, '[HEX+9]')
      # build the string backwards (right to left), byte by byte
      mov(:ecx, 4)
    end
    asm.emit_label(loop_label=asm.label)
    asm.block do
      # low nybble of nth byte
      movzx(:ebx, :al)
      and_(:bl, 0x0f)        # isolate low nybble
      movzx(:edx, 'byte [esi+ebx]')
      mov('byte [edi]', :dl)
      dec(:edi)
      # high nybble of nth byte
      movzx(:ebx, :al)
      and_(:bl, 0xf0)        # isolate high nybble
      shr(:bl, 4)
      mov(:dl, 'byte [esi+ebx]')
      mov('byte [edi]', :dl)
      dec(:edi)
      shr(:eax, 8)
      loop_(loop_label)
      # write(int fd, char *s, int n)
      mov(:eax, 4)              # SYS_write
      lea(:ecx, '[HEX]')        # ecx = &s
      args = [1,                # fd = 1 (STDOUT)
              :ecx,             # s = &s
              11]               # n = 11 (excluding term, max # of chars to print)
      case platform
      when 'darwin'             # on the stack, right to left (right @ highest addr)
        ####
        # setup bogus stack frame
        push(:ebp)
        mov(:ebp, :esp)
        sub(:esp, 36)
        ####
        args.reverse.each { |a| push(a) }
        push(:eax)
        int(0x80)
        ####
        # teardown bogus stack frame
        xor(:eax, :eax)
        add(:esp, 36)
        pop(:ebx)
        emit("leave")
        ####
      when 'linux'
        mov(:ebx, args[0])
        mov(:ecx, args[1])
        mov(:edx, args[2])
        int(0x80)
      end
    end
  end


############
# internal #
############


  def eof?
    @input.eof? && @look.nil?
  end

  Ops = {
    :add    => %w[+ -],
    :mul    => %w[* /],
    :rel    => %w[== != < > <= >=],
    :or     => %w[||],
    :and    => %w[&&],
    :bitor  => %w[| ^],
    :bitand => %w[&],
    :unary  => %w[- +]
  }
  # Op chars are chars that can begin an op, so OpChars needs to be a
  # map of kinds of operators to a list of valid prefix chars.
  OpChars = Ops.inject({}) { |hash, kv|
    key, val = *kv
    hash[key] = val.map {|op| op[0, 1]} # slice off first char for each op
    hash
  # Include :all for a very general test.
  }.merge(:all => Ops.values.flatten.map{|op| op[0, 1]}.sort.uniq)

  def op_char?(char, kind=:all)
    OpChars[kind].include?(char)
  end

  def op?(kind, token)
    Ops[kind].include?(token)
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
    got = options.has_key?(:got) ? options[:got] : @value
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
    alpha?(char) || digit?(char) || char == '_'
  end

  # XXX disabled! ... should treat true/false as constants
  def boolean?(char)
    char == 't' || char == 'f'
    false
  end

  def whitespace?(char)
    char == ' ' || char == "\t"
  end

  def newline?(char)
    char == "\n" || char == "\r"
  end

  def any_whitespace?(char)
    whitespace?(char) || newline?(char)
  end

  # Parse one or more newlines.
  def get_newline
    expected(:newline, :got => @look) unless newline?(@look)
    many(:newline?)
    @token = :newline
    @value = "\n"
  end

  # Match literal input.
  def match(char)
    expected(char, :got => @look) unless @look == char
    # puts "[ch] #{indent}#{char}"
    get_char
    skip_whitespace
  end

  # Match literal input.
  def match_word(word, options={})
    scan if options[:scan]
    expected(word) unless @value == word
  end

  # Parse zero or more consecutive characters for which the test is
  # true.
  def many(test)
    test = method(test) if test.is_a?(Symbol)
    token = ''
    while !eof? && test[@look]
      token << @look
      get_char
    end
    skip_whitespace
    token
  end

  # Parse a "name" (keyword or identifier).
  def get_name
    expected(:identifier) unless alpha?(@look)
    @value = many(:alnum?)
    @token = Keywords.include?(@value) ? :keyword : :identifier
    @value
  end

  # Parse a number.
  def get_number
    expected(:integer) unless digit?(@look)
    @token = :number
    @value = many(:digit?)
    # puts "[nu] #{indent}#{@value} (0x#{@value.to_i.to_s(16)})"
    @value
  end

  def get_boolean
    get_name
    expected(:boolean) unless @value == 'true' || @value == 'false'
    @token = :boolean
    # puts "[bo] #{indent}#{@value}"
    @value
  end

  def get_op
    expected(:operator) unless op_char?(@look)
    @token = :op
    @value = many(:op_char?)
  end

  # Skip leading whitespace.
  def skip_whitespace
    get_char while whitespace?(@look)
  end

  # Skip leading whitespace including newlines.
  def skip_any_whitespace
    get_char while any_whitespace?(@look)
  end

  
  def indent
    real_indent = if @value == 'else' || @value == 'end'
                    @indent - 1
                  else
                    @indent
                  end
    ' ' * (real_indent * 4)
  end

  def pushing(reg)
    asm.push(reg)
    yield
    asm.add(:esp, 4)
  end

  def op(name)
    pushing(:eax) do
      get_op
      expected(name) unless match_word(name)
      yield
    end
  end


  class <<self
    def hook(callback, *methods)
      methods.each do |m|
        orig = :"orig_#{m}"
        alias_method orig, m
        define_method(m) do
          val = send(orig)
          send(callback)
          val
        end
      end
    end
  end

  def print_token
    print(case @token
          when :keyword: '[kw] '
          when :number: '[nu] '
          when :identifier: '[id] '
          when :op: '[op] '
          when :boolean: '[bo] '
          when :newline: ''
          else
            raise "print doesn't know about #{@token}: #{@value}"
          end)
    print indent
    puts @value
  end

  # hook(:print_token,
  #   :get_name, :get_newline, :get_number, :get_op, :get_boolean)

end
