sjs<br>
[sami@samhuri.net](mailto:sami@samhuri.net)<br>

published : 2009-09-22<br>
updated   : 2026-09-08


Overview
========

A compiler for fun and education.  Written (mostly) in Ruby and based
on the tutorial "Let's Build a Compiler" by Jack Crenshaw[1].

[1]: http://compilers.iecc.com/crenshaw/

The semantics are simple and familiar to most programmers.  Eager
evaluation, assignment with the equals sign, arithmetic using + - *
and /, loops, if/else statement, etc.  Integers are the only data
type.

While the parser still closely resembles Crenshaw's recursive descent
parser, back-end generates x86 machine code using a homegrown
assembler in ~1000 lines of Ruby (just 650 lines of real code).

NOTE: The homegrown assembler writes ELF object files for Linux and
Mach-O object files for OS X, but the latter need a 32-bit OS X
toolchain that no longer exists.  The compiler can also emit assembly
text and leave the encoding to nasm, and the test suite runs both ways
on Linux.


Pre-requisites
==============

OS X
----

You need Ruby and gcc.  Ruby is standard on Macs but you'll need to
install Xcode for gcc.  You can also compile it yourself or use
MacPorts, or [homebrew](http://github.com/mxcl/homebrew).

Linux
-----

You need Ruby, nasm, and ld - which lives in the binutils package.

    % sudo apt install ruby nasm binutils

That's it!


Compiling
=========

The build script should detect your platform.  If not append 'elf' or
'macho' to the command.  The last argument picks the back-end: 'asm'
(the default) generates assembly for nasm, 'bin' encodes the machine
code and writes the object file in Ruby.

    % ./build.rb filename.code [outdir] [elf | macho] [asm | bin]

The resulting native executable is called 'filename' and you should be
able it run it directly.

    % ./filename

Run the test suite with make, through either back-end.

    % make test
    % make test FORMAT=bin


Syntax in 2 minutes
===================

The parser starts by parsing a block of code.  A block consists of one
or more statements.  Whitespace is largely ignored beyond delimiting
tokens, so statements can be grouped on one line or spread out over
multiple lines.  With no explicit terminator this can look strange so
we will see how it works out when the syntax evolves into something
more complicated.

There are variables and integers.  That's honestly about it.  There
are no functions or function calls, no closures, arrays, hashes, or
anything else.

Supported statements are:

 * assignment<br>   e.g. foo = 4096
 * if/else<br>      e.g. if x < 0 a=0 else a=1 end
 * while<br>        e.g. while x > 0 x=x-1 end
 * until<br>        e.g. until x == 0 x=x-1 end
 * break<br>        e.g. break
 * repeat<br>       e.g. repeat x=x-1 if x == 0 break end end
 * for<br>          e.g. for i=1 to 5 x=x+1 end
 * do<br>           e.g. do 5 x=x+1 end
 * print &lt;expr&gt;<br>        e.g. print 42

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

As far as booleans go, 0 is false and everything else is true.  Right
now there are only integers so this makes sense.


Internals
=========

It wasn't much fun generating assembly text, so I wrote an x86
assembler library in Ruby.  It implements just the instructions needed
for this compiler and is by no means complete.  It only does 32-bit
and no prefixes are supported.  It's basically just a handful of
instructions and mod-rm encoding.  I use the system's linker and have
no intention of writing my own, don't worry!

The classes that write object files are asm/elffile.rb and
asm/machofile.rb.

The asm/ directory holds the assembler but also the ELF and Mach-O
code, for now.  This is my first assembler and first time working
with the x86 ISA, so it probably isn't great.  It outputs horribly
inefficient code and there are no optimizations.

I did not write this compiler with the intention of anyone else
reading it but there are a reasonable amount of comments.


What next?
==========

Whatever interests me really, I don't know yet..  Right now I need to
clean up some of the code, now that object files of any size can be
generated and tests pass again.


Happy hacking!<br>
-sjs