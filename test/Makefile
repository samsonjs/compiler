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
	@./test.rb lt bin $(BINFORMAT)

gt: test.rb test_gt.code
	@./test.rb gt bin $(BINFORMAT)

ge: test.rb test_ge.code
	@./test.rb ge bin $(BINFORMAT)

le: test.rb test_le.code
	@./test.rb le bin $(BINFORMAT)

eq: test.rb test_eq.code
	@./test.rb eq bin $(BINFORMAT)

neq: test.rb test_neq.code
	@./test.rb neq bin $(BINFORMAT)

if: test.rb test_if.code
	@./test.rb if bin $(BINFORMAT)

while: test.rb test_while.code
	@./test.rb while bin $(BINFORMAT)

until: test.rb test_until.code
	@./test.rb until bin $(BINFORMAT)

repeat: test.rb test_repeat.code
	@./test.rb repeat bin $(BINFORMAT)

for: test.rb test_for.code
	@./test.rb for bin $(BINFORMAT)

do: test.rb test_do.code
	@./test.rb do bin $(BINFORMAT)

break: test.rb test_break.code
	@./test.rb break bin $(BINFORMAT)

print: test.rb test_print.code
	@./test.rb print bin $(BINFORMAT)

big: test.rb test_big.code
	@./test.rb big bin $(BINFORMAT)

huge: test.rb test_huge.code
	@./test.rb huge bin $(BINFORMAT)

or: test.rb test_or.code
	@./test.rb or bin $(BINFORMAT)

and: test.rb test_and.code
	@./test.rb and bin $(BINFORMAT)

bit_ops: test.rb test_bit_ops.code
	@./test.rb bit_ops bin $(BINFORMAT)

clean:
	@rm -f bin/*

.PHONY: clean