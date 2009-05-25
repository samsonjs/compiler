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

require 'asm'
require 'opcode'

class ParseError < StandardError
  attr_reader :caller, :context
  def initialize(caller, context=nil)
    @caller = caller
    @context = context
  end
end

class Compiler
  # This module uses our `emit_byte` method to output x86 machine code
  # directly using the assembler library.
  # include Assembler::Binary

  Keywords = %w[
    if else end while until repeat for to do break
    print
  ]
  
  attr_reader :data, :bss, :code

  def initialize(input, asm=Assembler::Text.new)
    # XXX for development only!
    @indent = 0                  # for pretty printing

    @look = ''                   # Next lookahead char.
    @token = nil                 # Type of last read token.
    @value = nil                 # Value of last read token.
    @input = input               # Stream to read from.
    @data = ''                   # Data section.
    @bss = ''                    # BSS section.
    @code = ''                   # Code section.
    @binary = []                 # Byte array of machine code.
    @vars = {}                   # Symbol table, maps names to locations in BSS.
    @num_labels = 0              # Used to generate unique labels.
    @num_labels_with_suffix = Hash.new(0)

    @header_size = 0x100                     # ELF, Linux, x86
    @text_offset = 0x08048000 + @header_size # Offset of text section in memory (Linux, x86).
    @text_size = 0x02be00                    # Size of text section.
    @data_offset = @text_offset + @text_size # Offset of data section.
    @data_size = 0x4e00                      # Size of data section.
    @bss_offset = @data_offset + @data_size  # Offset of bss section.
    @bss_size = 0                            # Size of bss section.

    # Labels for the assembler.  Maps names to locations.
    @labels = Hash.new {|h, key| raise "undefined label: #{key}"}

    @asm = asm

    # seed the lexer
    get_char
  end

  def asm
    @asm
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

  # a: [esp]
  # b: eax
  # 
  # If b - a is zero then a = b, and make_boolean will leave the zero
  # to effectively return false.  If b - a is non-zero then a != b,
  # and make_boolean will leave -1 (true) for us in eax.
  def neq_relation
    expression
    x86_sub(:eax, '[esp]')
    make_boolean
  end

  # Invert the != test for equal.
  def eq_relation
    neq_relation
    x86_not(:eax)
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

    true_label = unique_label(:cmp)
    end_label = unique_label(:endcmp)
    x86_cmp(a, b)
    x86_jl(true_label)

    x86_xor(:eax, :eax)         # return false
    x86_not(:eax) if invert     # (or true if inverted)
    x86_jmp(end_label)

    emit_label(true_label)
    x86_xor(:eax, :eax)          # return true
    x86_not(:eax) unless invert  # (or false if inverted)

    emit_label(end_label)
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

  # Used to implement the Two-Label-Loops (while, until, repeat).
  # 
  # name:  Name of the loop for readable labels.
  # block: Code to execute at the start of each iteration. (e.g. a
  #        condition)
  def simple_loop(name)
    start_label = unique_label(:"loop_#{name}")
    end_label = unique_label(:"end_#{name}")
    emit_label(start_label)

    yield(end_label)

    @indent += 1
    block(end_label)
    @indent -= 1
    match_word('end')
    x86_jmp(start_label)
    emit_label(end_label)
  end

  def while_stmt
    simple_loop('while') do |end_label|
      condition
      skip_any_whitespace
      x86_jz(end_label)
    end
  end

  def until_stmt
    simple_loop('until') do |end_label|
      condition
      skip_any_whitespace
      x86_jnz(end_label)
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
    x86_sub(:eax, 1)            # pre-decrement because of the
                                # following pre-increment
    x86_mov(counter, :eax)      # stash the counter in memory
    match_word('to', :scan => true)
    boolean_expression          # final value
    skip_any_whitespace
    x86_push(:eax)              # stash final value on stack
    final = '[esp]'

    simple_loop('for') do |end_label|
      x86_mov(:ecx, counter)      # get the counter
      x86_add(:ecx, 1)            # increment
      x86_mov(counter, :ecx)      # store the counter
      x86_cmp(final, :ecx)        # check if we're done
      x86_jz(end_label)           # if so jump to the end
    end

    x86_add(:esp, 4)            # clean up the stack
  end

  # do 5
  #   ...
  # end
  def do_stmt

    boolean_expression
    skip_any_whitespace
    x86_mov(:ecx, :eax)
    x86_push(:ecx)

    start_label = unique_label(:do)
    end_label = unique_label(:enddo)
    emit_label(start_label)

    x86_push(:ecx)

    @indent += 1
    block(end_label)
    @indent -= 1

    x86_pop(:ecx)

    match_word('end')
    x86_loop(start_label)

    # Phony push!  break needs to clean up the stack, but since we
    # don't know if there is a break at this point we fake a push and
    # always clean up the stack after.
    x86_sub(:esp, 4)

    emit_label(end_label)

    # If there was a break we have to clean up the stack here.  If
    # there was no break we clean up the phony push above.
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


  # Define a constant in the .data section.
  def equ(name, value)
    @data << "#{name}\tequ  #{value}"
  end

  # Define a variable with the given name and size (in dwords).
  def defvar(name, dwords=1)
    unless var?(name)
      @bss << "#{name}: resd #{dwords}\n"
      @vars[name] = @bss_size
      @bss_size += dwords
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

  # Emit a line of code wrapped between a tab and a newline.  Required
  # by Assembler::Text.
  def emit(code, options={})
    tab = options.has_key?(:tab) ? options[:tab] : "\t"
    @code << "#{tab}#{code}\n"
  end

  # emit_byte and bytes_written are required by Assembler::Binary.
  def emit_byte(byte)
    @binary << byte
  end
  def bytes_written
    @binary.size
  end


  def emit_label(name=unique_label)
    emit("#{name}:", :tab => nil)

    @labels[name] = @binary.length
  end

  def resolve_label(label)
    @labels[label]
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

  # Pack the array into a byte string.
  def binary
    @binary.pack('c*')
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

end
