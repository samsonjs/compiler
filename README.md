sjs<br>
[sami.samhuri@gmail.com](mailto:sami.samhuri@gmail.com)<br>

published : 2009-09-22<br>
updated   : 2009-09-24


Overview
========

A compiler for fun and education.  Written (mostly) in Ruby and based
on the tutorial "Let's Build a Compiler" by Jack Crenshaw[1].

[1]: http://compilers.iecc.com/crenshaw/

The semantics are simple and familiar to all programmers.  Eager
evaluation, assignment with the equals sign, arithmetic using + - *
and /, loops, if/else statement, etc.  Integers are the only data type.


NOTE: OS X is the only platform that compiles working binaries right
now.  ELF support for Linux coming soon.


Compiling
========

OS X
----

You need gcc, so install Xcode or use MacPorts to build gcc.


Linux
-----

You need ruby and ld which lives in the binutils package.

    % sudo aptitude install ruby binutils

That's it!  The assembler is included in ~900 lines of Ruby (including
comments).


You should be fine letting the build script detect your platform.  If
not append 'elf' or 'macho' to the command.

    % ./build.rb filename.code [elf | macho]

The resulting native executable will be called 'filename' and you
should be able to run it directly.

    % ./filename <return>



Syntax in 2 minutes
===================

The recursive descent parser starts by parsing a block of code.  A
block consists of zero or more statements.  Whitespace is largely
ignored beyond delimiting tokens so statements can be grouped on one
line or spread out over multiple lines.  With no explicit terminator
this can look strange so we will see how it works out when the syntax
evolves into something more complicated.

There are no functions or function calls, no closures, arrays, hashes,
or anything else you can think of.

Supported statements are:

 * assignment
   e.g. foo = 4096

 * if/else
   e.g. if x < 0 a=0 else a=1 end

 * while
   e.g. while x > 0 x=x-1 end

 * until
   e.g. until x == 0 x=x-1 end

 * break
   e.g. break

 * repeat
   e.g. repeat x=x-1 if x == 0 break end end

 * for
   e.g. for i=1 to 5 x=x+1 end

 * do
   e.g. do 5 x=x+1 end

 * print
   e.g. a=1 print

Print is strange, it prints the last value calculated in hex and that
is all.  Please don't look at the implementation. ;-)

Supported operations are the following, in increasing order of
precedence:

 * add + and subtract -
 * multiply * and divide /
 * relations: == != < > <= >=
 * boolean not !
 * [unimplemented] or ||
 * [unimplemented] and &&
 * bit or | and bit xor ^
 * bit and &
 * unary plus + and minus -

Parentheses are used to force a specific order of evaluation.

As far as booleans go 0 is false and everything else is true.  Right
now there are only integers so this makes sense.


Internals
=========

I wasn't satisfied using an external assembler and outputing assembly
text so I wrote an x86 assembler in Ruby.  It assembles just the
instructions I need for this compiler, so it is by no means complete.
32-bit only and no prefixes are supported.  It's basically just a
handful of instructions and mod-rm encoding.  I use the system's
linker and have no intention of writing my own, don't worry!

The code currently consists of a recursive descent parser that outputs
x86 code in ELF binaries on Linux and Mach-O binaries on Darwin.
Most of the code for outputing executables is Ruby, but ELF support is
still in C and not published in the repository.  Classes to output
Mach-O and ELF binaries are found in asm/(elf|macho)writer.rb, but ELF
support is not implemented yet so binaries only compile and run on OS
X right now.  ELF should come soon as I now have lights in my den. :)

Some major refactoring is needed as the project grew organically and
in order to keep up with the tutorials I have not yet made radical
changes.  The asm/ directory holds the assembler but also the MachO
and ELF code, for now.  The assembler is a from-scratch implementation
in Ruby. This is my first assembler and first time working with the
x86 ISA, so it probably isn't great.  It outputs horribly inefficient
code and there are no optimizations.

Hopefully I can reduce the number of lines by factoring more, but it's
pretty slim at ~3000 LOC.  About 2100 of those are actual code.  I did
not write this compiler with the intention of anyone else reading it
but there are a reasonable amount of comments.

<table>
  <tr>
    <th>Real Lines</th>
    <th>Total Lines</th>
    <th>Filename</th>
  </tr>

  <tr>
    <td>87</td>
    <td>112</td>
    <td>build.rb</td>
  </tr>
  <tr>
    <td>617</td>
    <td>891</td>
    <td>compiler.rb</td>
  </tr>
  <tr>
    <td>12</td>
    <td>29</td>
    <td>asm/asm.rb</td>
  </tr>
  <tr>
    <td>569</td>
    <td>843</td>
    <td>asm/binary.rb</td>
  </tr>
  <tr>
    <td>197</td>
    <td>319</td>
    <td>asm/cstruct.rb</td>
  </tr>
  <tr>
    <td>4</td>
    <td>6</td>
    <td>asm/elfsymtab.rb</td>
  </tr>
  <tr>
    <td>4</td>
    <td>8</td>
    <td>asm/elfwriter.rb</td>
  </tr>
  <tr>
    <td>170</td>
    <td>374</td>
    <td>asm/machofile.rb</td>
  </tr>
  <tr>
    <td>95</td>
    <td>163</td>
    <td>asm/macho.rb</td>
  </tr>
  <tr>
    <td>19</td>
    <td>28</td>
    <td>asm/machosym.rb</td>
  </tr>
  <tr>
    <td>48</td>
    <td>77</td>
    <td>asm/machosymtab.rb</td>
  </tr>
  <tr>
    <td>19</td>
    <td>25</td>
    <td>asm/machowriter.rb</td>
  </tr>
  <tr>
    <td>16</td>
    <td>25</td>
    <td>asm/objwriter.rb</td>
  </tr>
  <tr>
    <td>20</td>
    <td>31</td>
    <td>asm/registers.rb</td>
  </tr>
  <tr>
    <td>42</td>
    <td>66</td>
    <td>asm/regproxy.rb</td>
  </tr>
  <tr>
    <td>56</td>
    <td>89</td>
    <td>asm/symtab.rb</td>
  </tr>
  <tr>
    <td>131</td>
    <td>183</td>
    <td>asm/text.rb</td>
  </tr>
  <tr>
    <td>2097</td>
    <td>3269</td>
    <td><b>total</b></td>
  </tr>
</table>


Happy hacking!
-sjs