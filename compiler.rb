# A compiler as described by Jack Crenshaw in his famous book "Let's
# Build a Compiler".  At least in the beginning, this code will
# closely reflect the Pascal code written by Jack.  Over time it may
# become more idiomatic, however this is an academic exercise.
#
# sjs
# may 2009

require 'rubygems'
require 'unroller'

class ParseError < StandardError
  attr_reader :caller, :context
  def initialize(caller, context=nil)
    @caller = caller
    @context = context
  end
end

class Compiler
  attr_reader :data, :bss, :code

  Keywords = %w[
    if else end while until repeat for to do break
    print
  ]
  
  def initialize(input=STDIN)
    @look = ''                   # lookahead char
    @token = nil                 # type of last read token
    @value = nil                 # value of last read token
    @input = input               # stream to read from
    @data = ''                   # data section
    @bss = ''                    # bss section
    @code = ''                   # code section
    @vars = {}                   # defined variables
    @num_labels = 0              # used to generate unique labels
    @num_labels_with_suffix = Hash.new(0)
    @indent = 0                  # for pretty printing

    # seed the lexer
    get_char
  end

  def parse
    block
    expected(:'end of file') unless eof?
    [@data, @bss, @code]
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
      x86_mov(:eax, get_number.to_i)
    else
      expected(:'integer, identifier, function call, or parenthesized expression', :got => @look)
    end
  end

  # Parse a signed factor.
  def signed_factor
    sign = @look
    match(sign) if op?(:unary, sign)
    factor
    x86_neg(:eax) if sign == '-'
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


  ###################
  # bit expressions #
  ###################

  def bitor_expr
    match('|')
    term
    x86_or(:eax, '[esp]')
  end

  def bitand_expr
    match('&')
    signed_factor
    x86_and(:eax, '[esp]')
  end

  def xor_expr
    match('^')
    term
    x86_xor(:eax, '[esp]')
  end


  #######################
  # boolean expressions #
  #######################

  def boolean_expression
    boolean_term
    while @look == '|'
      op '||' do
        boolean_term
        emit("<logical or>")
      end
    end
  end

  def boolean_term
    not_factor
    while @look == '&'
      op '&&' do
        not_factor
        emit("<logical and>")
      end
    end
  end

  def boolean_factor
    if boolean?(@look)
      if get_boolean == 'true'
        x86_mov(:eax, -1)
      else
        x86_xor(:eax, :eax)
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

  def eq_relation
    expression
    x86_sub(:eax, '[esp]')
    make_boolean
    x86_not(:eax)
  end

  def neq_relation
    expression
    x86_sub(:eax, '[esp]')
    make_boolean
  end

  def gt_relation
    gt_label = unique_label(:gt)
    end_label = unique_label(:endgt)
    expression
    x86_cmp(:eax,  '[esp]')         # b - a < 0 if a > b
    x86_jl(gt_label)
    x86_xor(:eax, :eax)
    x86_jmp(end_label)
    emit_label(gt_label)
    x86_xor(:eax, :eax)
    x86_not(:eax)
    emit_label(end_label)
  end

  def lt_relation
    lt_label = unique_label(:lt)
    end_label = unique_label(:endlt)
    expression
    x86_cmp('[esp]', :eax)         # a - b < 0 if a < b
    x86_jl(lt_label)
    x86_xor(:eax, :eax)
    x86_jmp(end_label)
    emit_label(lt_label)
    x86_xor(:eax, :eax)
    x86_not(:eax)
    emit_label(end_label)
  end

  # def ge_relation
  #   ge_label = unique_label(:ge)
  #   end_label = unique_label(:endge)
  #   expression
  #   x86_cmp(:eax,  '[esp]')         # b - a < 0 if a > b
  #   x86_jl(gt_label)
  #   x86_xor(:eax, :eax)
  #   x86_jmp(end_label)
  #   emit_label(gt_label)
  #   x86_xor(:eax, :eax)
  #   x86_not(:eax)
  #   emit_label(end_label)
  # end

  # def lt_relation
  #   lt_label = unique_label(:lt)
  #   end_label = unique_label(:endlt)
  #   expression
  #   x86_cmp('[esp]', :eax)         # a - b < 0 if a < b
  #   x86_jl(lt_label)
  #   x86_xor(:eax, :eax)
  #   x86_jmp(end_label)
  #   emit_label(lt_label)
  #   x86_xor(:eax, :eax)
  #   x86_not(:eax)
  #   emit_label(end_label)
  # end


  ######################################
  # statements and controls structures #
  ######################################

  # Parse an assignment statement.  Value is in eax.
  def assignment
    name = @value
    match('=')
    boolean_expression
    defvar(name) unless var?(name)
    x86_mov("dword [#{name}]", :eax)
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
    else_label = unique_label(:end_or_else)
    end_label = else_label      # only generated if else clause
                                # present
    condition
    skip_any_whitespace
    x86_jz(else_label)
    @indent += 1
    block(label)
    @indent -= 1
    if @token == :keyword && @value == 'else'
      skip_any_whitespace
      end_label = unique_label(:endif) # now we need the 2nd label
      x86_jmp(end_label)
      emit_label(else_label)
      @indent += 1
      block(label)
      @indent -= 1
    end
    match_word('end')
    emit_label(end_label)
  end

  def while_stmt
    while_label = unique_label(:while)
    end_label = unique_label(:endwhile)
    emit_label(while_label)
    condition
    skip_any_whitespace
    x86_jz(end_label)
    @indent += 1
    block(end_label)
    @indent -= 1
    match_word('end')
    x86_jmp(while_label)
    emit_label(end_label)
  end

  def until_stmt
    until_label = unique_label(:until)
    end_label = unique_label(:enduntil)
    emit_label(until_label)
    condition
    skip_any_whitespace
    x86_jnz(end_label)
    @indent += 1
    block(end_label)
    @indent -= 1
    match_word('end')
    x86_jmp(until_label)
    emit_label(end_label)
  end

  def repeat_stmt
    skip_any_whitespace         # no condition, slurp whitespace
    repeat_label = unique_label(:repeat)
    end_label = unique_label(:endrepeat)
    emit_label(repeat_label)
    @indent += 1
    block(end_label)
    @indent -= 1
    match_word('end')
    x86_jmp(repeat_label)
    emit_label(end_label)
  end

  # s = 0
  # f x = 1 to 5
  #   s = s + x
  # e
  def for_stmt
    start_label = unique_label(:for)
    end_label = unique_label(:endfor)
    counter = "[#{get_name}]"
    match('=')
    boolean_expression          # initial value
    x86_sub(:eax, 1)            # pre-decrement because of the
                                # following pre-increment
    x86_mov(counter, :eax)      # stash the counter in memory
    match_word('to', :scan => true)
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
    @indent += 1
    block(end_label)            # otherwise execute the block
    @indent -= 1
    match_word('end')
    x86_jmp(start_label)        # lather, rinse, repeat
    emit_label(end_label)
    x86_add(:esp, 4)            # clean up the stack
  end

  # d 5
  #   ...
  # e
  def do_stmt
    start_label = unique_label(:do)
    end_label = unique_label(:enddo)
    boolean_expression
    skip_any_whitespace
    x86_mov(:ecx, :eax)
    x86_push(:ecx)
    counter = '[esp]'
    emit_label(start_label)
    x86_mov(counter, :ecx)
    @indent += 1
    block(end_label)
    @indent -= 1
    x86_mov(:ecx, counter)
    match_word('end')
    x86_loop(start_label)
    x86_sub(:esp, 4)
    emit_label(end_label)
    x86_add(:esp, 4)
  end

  def break_stmt(label)
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

  # print eax in hex format
  def print_stmt
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
    while test[@look]
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

  def indent
    real_indent = if @value == 'else' || @value == 'end'
                    @indent - 1
                  else
                    @indent
                  end
    ' ' * (real_indent * 4)
  end

  def pushing(reg)
    x86_push(reg)
    yield
    x86_add(:esp, 4)
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


  ######################
  # assembler routines #
  ######################

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
