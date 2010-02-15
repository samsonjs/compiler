PLATFORM=$(shell uname -s)
BINFORMAT="bin"
ifeq ($(PLATFORM), Darwin)
	BINFORMAT="macho"
endif
ifeq ($(PLATFORM), Linux)
	BINFORMAT="elf"
endif
ifeq ($(BINFORMAT), bin)
	@echo "WARNING: binary format is 'bin', this is probably not what you want!"
	@echo "         Your platform, " $(PLATFORM) ", is unsupported."
endif

all: lt gt ge le eq neq or and if while until repeat for do break print big huge
	@true

lt: test.rb test_lt.code
	@./test.rb lt $(BINFORMAT)

gt: test.rb test_gt.code
	@./test.rb gt $(BINFORMAT)

ge: test.rb test_ge.code
	@./test.rb ge $(BINFORMAT)

le: test.rb test_le.code
	@./test.rb le $(BINFORMAT)

eq: test.rb test_eq.code
	@./test.rb eq $(BINFORMAT)

neq: test.rb test_neq.code
	@./test.rb neq $(BINFORMAT)

if: test.rb test_if.code
	@./test.rb if $(BINFORMAT)

while: test.rb test_while.code
	@./test.rb while $(BINFORMAT)

until: test.rb test_until.code
	@./test.rb until $(BINFORMAT)

repeat: test.rb test_repeat.code
	@./test.rb repeat $(BINFORMAT)

for: test.rb test_for.code
	@./test.rb for $(BINFORMAT)

do: test.rb test_do.code
	@./test.rb do $(BINFORMAT)

break: test.rb test_break.code
	@./test.rb break $(BINFORMAT)

print: test.rb test_print.code
	@./test.rb print $(BINFORMAT)

big: test.rb test_big.code
	@./test.rb big $(BINFORMAT)

huge: test.rb test_huge.code
	@./test.rb huge $(BINFORMAT)

or: test.rb test_or.code
	@./test.rb or $(BINFORMAT)

and: test.rb test_and.code
	@./test.rb and $(BINFORMAT)

bit_ops: test.rb test_bit_ops.code
	@./test.rb bit_ops $(BINFORMAT)

clean:
	@rm -f test*.asm test*.o
	@rm -f test_big test_lt test_gt test_ge test_le test_eq test_neq
	@rm -f test_while test_if test_until test_repeat test_do
	@rm -f test_for test_break test_print test_huge
	@rm -f test_and test_or test_bit_ops

.PHONY: clean