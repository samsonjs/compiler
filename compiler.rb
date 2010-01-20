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

require 'asm/registers'
require 'asm/varproxy'

class ParseError < StandardError
  attr_reader :caller, :context
  def initialize(caller, context=nil)
    @caller = caller
    @context = context
  end
end

class Compiler

  include Assembler::Registers

  Keywords = %w[
    if else end while until repeat for to do break
    print
  ]
  
  attr_reader :asm

  def initialize(input, asm)
    @indent = 0                  # for pretty printing
    @look = ''                   # Next lookahead char.
    @token = nil                 # Type of last read token.
    @value = nil                 # Value of last read token.
    @input = input               # Stream to read from.
    @asm = asm                   # assembler

    # seed the lexer
    get_char
  end

  def compile
    block # parse a block of code
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
      asm.mov(EAX, [asm.var(name)])
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
      asm.mov(EAX, get_number.to_i)
    else
      expected(:'integer, identifier, function call, or parenthesized expression', :got => @look)
    end
  end

  # Parse a signed factor.
  def signed_factor
    sign = @look
    match(sign) if op?(:unary, sign)
    factor
    asm.neg(EAX) if sign == '-'
  end

  # Parse and translate a single term (factor or mulop).  Result is in
  # eax.
  def term
    signed_factor                      # Result in eax.

    while op?(:mul, @look)
      asm.push(EAX)
      case @look
      when '*': multiply
      when '/': divide
      end
    end
  end

  # Parse and translate a general expression of terms.  Result is
  # in eax.
  def expression
    term                      # Result is in eax.

    while op_char?(@look, :add)
      asm.push(EAX)
      case @look
      when '+': add
      when '-': subtract
      end
    end
  end

  # Parse an addition operator and the 2nd term (b).  The result is
  # left in eax.  The 1st term (a) is expected on the stack.
  def add
    match('+')
    term                        # Result is in eax.
    asm.pop(EBX)
    asm.add(EAX, EBX)         # Add a to b.
  end

  # Parse a subtraction operator and the 2nd term (b).  The result is
  # left in eax.  The 1st term (a) is expected on the stack.
  def subtract
    match('-')
    term                      # Result, b, is in eax.
    asm.pop(EBX)
    asm.neg(EAX)              # Fake the subtraction.  a - b == a + -b
    asm.add(EAX, EBX)         # Add a(ebx) to -b(eax).
  end

  # Parse an addition operator and the 2nd term (b).  The result is
  # left in eax.  The 1st term (a) is expected on the stack.
  def multiply
    match('*')
    signed_factor               # Result is in eax.
    asm.pop(EBX)
    asm.imul(EBX)             # Multiply a by b.
  end

  # Parse a division operator and the divisor (b).  The result is
  # left in eax.  The dividend (a) is expected on the stack.
  def divide
    match('/')
    signed_factor               # Result is in eax.
    asm.pop(EBX)
    asm.xchg(EAX, EBX)          # Swap the divisor and dividend into
                                # the correct places.

    # idiv uses edx:eax as the dividend so we need to ensure that edx
    # is correctly sign-extended w.r.t. eax.
    asm.cdq              # Sign-extend eax into edx (Convert Double to
                         # Quad).
    asm.idiv(EBX)        # Divide a (eax) by b (ebx).
  end


  ###################
  # bit expressions #
  ###################

  def bit_expr(op, token)
    match(token)
    if block_given? then yield else term end
    asm.pop(EBX)
    asm.send(op, EAX, EBX)
  end

  def bitor_expr
    bit_expr(:or, '|')
  end

  def xor_expr
    bit_expr(:xor, '^')
  end

  def bitand_expr
    bit_expr(:and, '&') { signed_factor }
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
        asm.mov(EAX, -1)
      else
        asm.xor(EAX, EAX)
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
      make_boolean(EAX)        # ensure it is -1 or 0...
      asm.not_(EAX)            # so that 1's complement NOT is also boolean not
    else
      boolean_factor
    end
  end

  # Convert any identifier to a boolean (-1 or 0).  This is
  # semantically equivalent to !!reg in C or Ruby.
  def make_boolean(reg=EAX)
    end_label = asm.mklabel(:endmakebool)
    asm.cmp(reg, 0)             # if false do nothing
    asm.jz(end_label)
    asm.mov(reg, -1)            # truthy, make it true
    asm.deflabel(end_label)
  end

  def relation
    expression
    if op_char?(@look, :rel)
      scan
      asm.push(EAX)
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

  # a: <on the stack>
  # b: eax
  # 
  # If b - a is zero then a = b, and make_boolean will leave the zero
  # to effectively return false.  If b - a is non-zero then a != b,
  # and make_boolean will leave -1 (true) for us in eax.
  def neq_relation
    expression
    asm.pop(EBX)
    asm.sub(EAX, EBX)
    make_boolean
  end

  # Invert the != test for equal.
  def eq_relation
    neq_relation
    asm.not_(EAX)
  end

  # > and < are both implemented in terms of jl (jump if less than).
  # We exploit the fact that cmp is the subtraction of src from dest
  # and order the terms appropriately for each function.  As for >=
  # and <=, they in turn are implemented in terms of > and <.  a is
  # greater than or equal to b if and only if a is *not* less than b.
  #
  # Note: This was done to minimize the number of instructions that
  #       the assembler needed to implement, but since the Jcc
  #       instructions are very cheap to implement this is no longer
  #       a concern.
  
  
  # The next 4 relations all compare 2 values a and b, then return
  # true (-1) if the difference was below zero and false (0)
  # otherwise (using JL, jump if less than).
  def cmp_relation(a, b, options={})
    expression
    asm.pop(EBX)

    # Invert the sense of the test?
    invert = options[:invert]

    true_label = asm.mklabel(:cmp)
    end_label = asm.mklabel(:endcmp)
    asm.cmp(a, b)
    asm.jl(true_label)

    asm.xor(EAX, EAX)          # return false
    asm.not_(EAX) if invert    # (or true if inverted)
    asm.jmp(end_label)

    asm.deflabel(true_label)
    asm.xor(EAX, EAX)            # return true
    asm.not_(EAX) unless invert  # (or false if inverted)

    asm.deflabel(end_label)
  end

  # a: <on the stack>
  # b: eax
  #
  # if a > b then b - a < 0
  def gt_relation
    cmp_relation(EAX, EBX) # b - a
  end

  # a: <on the stack>
  # b: eax
  #
  # if a < b then a - b < 0
  def lt_relation
    cmp_relation(EBX, EAX) # a - b
  end

  # a: <on the stack>
  # b: eax
  #
  # if a >= b then !(a < b)
  def ge_relation
    # Compare them as in less than but invert the result.
    cmp_relation(EBX, EAX, :invert => true)
  end

  # a: <on the stack>
  # b: eax
  # 
  # if a <= b then !(a > b)
  def le_relation
    # Compare them as in greater than but invert the result.
    cmp_relation(EAX, EBX, :invert => true)
  end


  ######################################
  # statements and controls structures #
  ######################################

  # Parse an assignment statement.  Value is in eax.
  def assignment
    name = @value
    match('=')
    boolean_expression
    lval = asm.var!(name)
    asm.mov([lval], EAX)
  end

  # Parse a code block.
  #
  # TODO replace the case..when with a lookup table
  #      (might be exposed in the language later)
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
    else_label = asm.mklabel(:end_or_else)
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
      end_label = asm.mklabel(:endif) # now we need the 2nd label
      asm.jmp(end_label)
      asm.deflabel(else_label)
      @indent += 1
      block(label)
      @indent -= 1
    end
    match_word('end')
    asm.deflabel(end_label)
  end

  # Used to implement the Two-Label-Loops (while, until, repeat).
  # 
  # name:  Name of the loop for readable labels.
  # block: Code to execute at the start of each iteration. (e.g. a
  #        condition)
  def simple_loop(name)
    start_label = asm.mklabel(:"#{name}_loop")
    end_label = asm.mklabel(:"end_#{name}")
    asm.deflabel(start_label)

    yield(end_label)

    @indent += 1
    block(end_label)
    @indent -= 1
    match_word('end')
    asm.jmp(start_label)
    asm.deflabel(end_label)
  end

  def condition_loop(name, jump_instruction)
    simple_loop(name) do |end_label|
      condition
      skip_any_whitespace
      asm.send(jump_instruction, end_label)
    end
  end

  def while_stmt
    condition_loop('while', :jz) # done when == 0 (falsish)
  end

  def until_stmt
    condition_loop('until', :jnz) # done when != 0 (truthy)
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
    name = get_name
    counter = asm.defvar(name)
    match('=')
    boolean_expression                 # initial value
    asm.sub(EAX, 1)                    # pre-decrement because of the
                                       # following pre-increment
    asm.mov([counter], EAX)   # stash the counter in memory
    match_word('to', :scan => true)
    boolean_expression                 # final value
    skip_any_whitespace
    asm.push(EAX)                      # stash final value on stack
    final = [ESP]

    simple_loop('for') do |end_label|
      asm.mov(ECX, [counter]) # get the counter
      asm.add(ECX, 1)                  # increment
      asm.mov([counter], ECX) # store the counter
      asm.cmp(final, ECX)              # check if we're done
      asm.jz(end_label)                # if so jump to the end
    end

    asm.add(ESP, 4)                    # clean up the stack
  end

  # do 5
  #   ...
  # end
  def do_stmt

    boolean_expression
    skip_any_whitespace
    asm.mov(ECX, EAX)

    start_label = asm.mklabel(:do)
    end_label = asm.mklabel(:enddo)
    asm.deflabel(start_label)

    asm.push(ECX)

    @indent += 1
    block(end_label)
    @indent -= 1

    asm.pop(ECX)

    match_word('end')
    asm.dec(ECX)
    asm.jnz(start_label)

    # Phony push!  break needs to clean up the stack, but since we
    # don't know if there is a break at this point we fake a push and
    # always clean up the stack after.
    asm.sub(ESP, 4)

    asm.deflabel(end_label)

    # If there was a break we have to clean up the stack here.  If
    # there was no break we clean up the phony push above.
    asm.add(ESP, 4)
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
    asm.cmp(EAX, 0)            # 0 is false, anything else is true
  end

  # print eax in hex format
  def print_stmt
    # variables
    d = '__DIGITS'
    h = '__HEX'

    digits = if asm.var?(d)
               asm.var(d)
             else
               d_var = asm.defvar(d, 4)
               asm.block do
                 # define a lookup table of digits
                 mov([d_var],    0x33323130)
                 mov([d_var+4],  0x37363534)
                 mov([d_var+8],  0x62613938)
                 mov([d_var+12], 0x66656463)
               end
               d_var
             end

    # 3 dwords == 12 chars
    hex = asm.var!(h, 3)
    
    asm.block do
      # TODO check sign and prepend '-' if negative
      mov([hex], 0x7830)  # "0x" == [48, 120]
      mov([hex+10], 0xa)  # newline + null terminator
    end
    boolean_expression
    asm.block do
      # convert eax to a hex string
      lea(ESI, [digits])
      lea(EDI, [hex+9])
      # build the string backwards (right to left), byte by byte
      mov(ECX, 4)
    end
    asm.block do
      deflabel(loop_label=mklabel)
      # low nybble of nth byte
      movzx(EBX, AL)
      and_(BL, 0x0f)        # isolate low nybble
      movzx(EDX, [:byte, ESI+EBX])
      mov([EDI], DL)
      dec(EDI)
      # high nybble of nth byte
      movzx(EBX, AL)
      and_(BL, 0xf0)        # isolate high nybble
      shr(BL, 4)
      mov(DL, [ESI+EBX])
      mov([EDI], DL)
      dec(EDI)
      shr(EAX, 8)
      loop_(loop_label)
      # write(int fd, char *s, int n)
      mov(EAX, 4)               # SYS_write
      lea(ECX, [hex])           # ecx = &s
      args = [1,                # fd = 1 (STDOUT)
              ECX,              # s = &s
              11]               # n = 11 (excluding term, max # of chars to print)
      case platform
      when 'darwin'             # on the stack, right to left (right @ highest addr)
        ####
        # setup bogus stack frame
        push(EBP)
        mov(EBP, ESP)
        sub(ESP, 36)
        ####
        args.reverse.each { |a| push(a) }
        push(EAX)
        int(0x80)
        ####
        # teardown bogus stack frame
        xor(EAX, EAX)
        add(ESP, 36)
        pop(EBX)
        leave
        ####
      when 'linux'
        mov(EBX, args[0])
        mov(ECX, args[1])
        mov(EDX, args[2])
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
    asm.add(ESP, 4)
  end

  def op(name)
    asm.push(EAX)
    get_op
    expected(name) unless match_word(name)
    yield
    asm.add(ESP, 4)
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
