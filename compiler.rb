# A compiler as described by Jack Crenshaw in his famous book "Let's
# Build a Compiler".  At least in the beginning, this code will
# closely reflect the Pascal code written by Jack.  Over time it may
# become more idiomatic, however this is an academic exercise.
#
# sjs
# may 2009

require 'opcode'
#require 'assembler'

class ParseError < StandardError
  attr_reader :caller, :context
  def initialize(caller, context=nil)
    @caller = caller
    @context = context
  end
end

class Compiler
#  include Assembler
  
  attr_reader :data, :bss, :code

  def initialize(input=STDIN)
    @look = ''                   # Next lookahead char.
    @input = input               # Stream to read from.
    @data = ''                   # Data section.
    @bss = ''                    # BSS section.
    @code = ''                   # Code section.
    @vars = {}                   # Symbol table, maps names to locations in BSS.
    @num_labels = 0              # Used to generate unique labels.
    @num_labels_with_suffix = Hash.new(0)
    @break_stack = []            # for breaking out of loops
    @binary = []                 # Byte array of machine code.
    @machine_code = ''           # Byte string of machine code.

    @header_size = 0x100                     # ELF, Linux, x86
    @text_offset = 0x08048000 + @header_size # Offset of text section in memory (Linux, x86).
    @text_size = 0x02be00                    # Size of text section.
    @data_offset = @text_offset + @text_size # Offset of data section.
    @data_size = 0x4e00                      # Size of data section.
    @bss_offset = @data_offset + @data_size  # Offset of bss section.
    @bss_size = 0                            # Size of bss section.

    # Labels for the assembler.  Maps names to locations.
    @labels = Hash.new {|h, key| raise "undefined label: #{key}"}

    # Dispatch table for keywords.
    @dispatch = {
      'b' => method(:break_stmt),        # break
      'e' => nil,                        # end
      'l' => nil,                        # else
      'i' => method(:if_else_stmt),      # if-else
      'r' => method(:repeat_stmt),       # repeat
      'u' => method(:until_stmt),        # until
      'w' => method(:while_stmt)         # while
    }

    # Reserved words (... constant?)
    @keywords = @dispatch.keys

    # seed the lexer
    get_char
  end

  def parse
    block
    expected(:'end of file') unless eof?
    compile
    [@data, @bss, @code, @machine_code]
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
    if handler = @dispatch[@look]
      handler.call
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
    many(method(:digit?)).to_i
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
      @vars[name] = @bss_size
      @bss_size += dwords
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


  # x86 machine code generation

  def emit_byte(byte)
    @binary << byte
  end

  def emit_dword(num)
    @binary += num_to_quad(num)
  end

  # 0-2: r/m
  # 3-5: reg/opcode
  # 6-7: mod
  # 
  # dest and src are tuples of the form [type, value] where type is
  # any of :reg, :rm32, :imm32.  Max _one_ :rm32 arg per call.
  def emit_modrm(dest, src, override)
    if dest[0] == :reg
      reg = override[:op] || regnum(dest[1])

      # mod == 11 (register content)
      if src[0] == :reg
        mod = 3
        rm = regnum(src[1])

      # mod == 00 (pointer)
      elsif src[0] == :rm32
        mod = 0
        parts = decode_addr(src[1])
        rm = case parts[0]
             # mod == 00 (direct pointer e.g. [eax])
             when :reg
               regnum(parts[1])
             when :sib
               sib = parts[1..-1]
               4
             when :disp
               disp = parts[1]
               5
             end
      end
    elsif src[0] == :reg
      reg = override[:op] || regnum(src[1])
    else
      raise "unsupported mod r/m byte! dest=#{dest} src=#{src}"
    end
    emit_byte((mod << 6) & (reg << 3) & rm)
    emit_sib(sib) if defined? sib
    emit_dword(disp) if defined? disp
  end

  def emit_sib(sib)
    scale, index, base = *sib
    if [1,2,4,8].include?(scale)
      scale = log2(scale)
    else
      raise "unsupported SIB scale: #{scale}, should be [1, 2, 4, 8]"
    end
    emit_byte((scale << 6) & (index << 3) & base)
  end

  def compile
    @machine_code = @binary.pack('c*')
  end


  # Some asm methods for convenience and arity checks.  Now emits
  # some real machine code too.

  # This is the full set of x86 registers.
  # Registers = [:eax, :ecx, :edx, :ebx, :esp, :ebp, :esi, :edi]

  # This will do for early work.  
  # Position indicates value in op codes.
  Registers = [:eax, :ecx, :edx, :ebx]

  # Regex to match any x86 register name, and then some.  Should be
  # sufficient.
  RegisterRegex = 'e[acdbsd][xip]'

  # Match a literal number in binary, octal, decimal, or hex
  NumberRegex = '(0[xXbB]?)?[0-9a-fA-F]+'

  # Match a variable name.
  NameRegex = '[a-zA-Z][a-zA-Z0-9]*'

  # 0.size gives the real answer, we only do x86 though
  MachineBytes = 4    
  MachineBits = MachineBytes * 8
  MinSigned = -1 * 2**(MachineBits-1)
  MaxSigned = 2**(MachineBits-1) - 1
  MinUnsigned = 0
  MaxUnsigned = 2**MachineBits - 1
  SignedRange = MinSigned..MaxSigned

  # assemble x86 machine code
  def asm
    # stash the current number of bytes written
    instruction_offset = @binary.length

    yield

    # return the number of bytes written
    @binary.length - instruction_offset
  end

  def register?(op)
    Registers.index(op)
  end
  alias_method :regnum, :register?
  
  def immediate?(op)
    op.is_a?(Numeric) || (op.is_a?(String) && op.match(/^#{NumberRegex}$/))
  end

  def rm32?(op)
    offset?(op) || op.respond_to?(:match) && op.match(/^
      \[
       #{RegisterRegex}                       # base register
       (\+#{RegisterRegex}                    # optional index register
        (\*[1248])?                           # optional scale
       )?
      \]
      $/x)
  end

  # 6 versions of the mov instruction are supported:
  #   1.  mov reg32, immediate32 (0xb8+destreg, imm32)
  #   2.  mov reg32, r/m32 (0x8b, mod r/m, maybe sib)
  #   2a. mov eax, memoffset32 (0xa1, disp32)
  #   3.  mov r/m32, reg32 (0x89, mod r/m, maybe sib)
  #   3a. mov memoffset32, eax (0xa3, disp32)
  #   4.  mov r/m32, immediate32 (0xc7, mod r/m, maybe sib, imm32)
  def x86_mov(dest, src)
    emit("mov #{dest}, #{src}")

    dest = dest[6..-1] if dest.is_a?(String) && dest[0..5] == 'dword '
    src = src[6..-1] if src.is_a?(String) && src[0..5] == 'dword '

    asm do

      # version 1: mov r32, imm32
      if register?(dest) && immediate?(src)
          emit_byte(0xb8 + regnum(dest)) # dest encoded in instruction
          emit_dword(parse_num(src))

      # version 2: mov r32, r/m32
      elsif register?(dest) && rm32?(src)
        # version 2a: mov eax, moffs32
        if dest == :eax && offset?(src)
          emit_byte(0xa1)
          num = decode_addr(src)[1]
          emit_dword(num)
        else
          emit_byte(0x8b)
          emit_modrm([:reg, dest], [:rm32, src])
        end

      # version 3: mov r/m32, r32
      elsif rm32?(dest) && register?(src)
        # version 3a: mov moffs32, eax
        if offset?(dest) && src == :eax
          emit_byte(0xa3)
          num = decode_addr(dest)[1]
          emit_dword(num)
        else
          emit_byte(0x89)
          emit_modrm([:rm32, dest], [:reg, src])
        end

      # version 4: mov r/m32, imm32
      elsif rm32?(dest) && immediate?(src)
        emit_byte(0xc7)
        emit_modrm([:rm32, dest], [:imm32, src], :op => 0)
      else
        puts "rm32?(dest): #{rm32?(dest)}\t\trm32?(src): #{rm32?(src)}"
        puts "register?(dest): #{register?(dest)}\t\tregister?(src): #{register?(src)}"
        puts "immediate?(dest): #{immediate?(dest)}\t\timmediate?(src): #{immediate?(src)}"
        puts "offset?(dest): #{offset?(dest)}\t\toffset?(src): #{offset?(src)}"
        #raise "unsupported mov format: mov #{dest}, #{src}"
        puts "!!! unsupported mov format: mov #{dest}, #{src}"
      end

    end # asm do

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

    asm do
      if register?(op)
        emit_byte(0x40 + regnum(op))
      elsif rm32?(op)
        emit_byte(0xff)
        emit_modrm(
      else
        raise "unsupported op #{op}, wanted r32 or r/m32"
      end
    end
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


  def offset?(addr)
    addr.respond_to?(:match) && addr.match(/^\[(#{NameRegex}|#{NumberRegex})\]$/)
  end

  def decode_addr(addr)
    addr = addr[1..-2]   # strip brackets

    if matches = addr.match(/^#{NameRegex}$/)
      unless loc = @vars[matches[0]]
        raise "undefined variable #{matches[0]}"
      end
      [:disp, @bss_offset + loc]
    elsif matches = addr.match(/^#{NumberRegex}$/)
      [:disp, parse_num(matches[0])]
    elsif addr.index('*')
      bi, scale = *addr.split('*')
      base, index = *bi.split('+')
      [:sib, scale.to_i, index.to_sym, base.to_sym]
    elsif addr.index('+')
      base, index = *addr.split('+')
      [:sib, 1, index.to_sym, base.to_sym]
    else
      [:reg, addr.to_sym]
    end
  end
  
  # Parse a number from a string.  Used by emit_dword.
  def parse_num(str)
    # If it's not a string it's a number, just return it.
    return str unless str.is_a?(String)

    str.downcase!
    base = 10                   # default to base 10
    if str[0, 1] == '0'
      base = case str[1, 1]
             when 'x'
               16
             when 'b'
               str.slice!(2..-1)
               2
             else
               8
             end
    end
    str.to_i(base)
  end

  # Convert a number to a quad of bytes, discarding excess bits.
  # Little endian!
  def num_to_quad(num)
    [
     num & 0xff,
     (num >>  8) & 0xff,
     (num >> 16) & 0xff,
     (num >> 24) & 0xff
    ]
  end

  def log2(x, tol=1e-13)
    result = 0.0
 
    # Integer part
    while x < 1
      resultp -= 1
      x *= 2
    end
    while x >= 2
      result += 1
      x /= 2
    end
 
    # Fractional part
    fp = 1.0
    while fp >= tol
      fp /= 2
      x *= x
      if x >= 2
        x /= 2
        result += fp
      end
    end 
    result
  end

end
