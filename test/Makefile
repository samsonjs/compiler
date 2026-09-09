PLATFORM=$(shell uname -s)
BINFORMAT="bin"
FORMAT=bin
FIXTURES=lt gt ge le eq neq or and bit_ops if while until repeat for do break print big huge
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

all: $(FIXTURES)
	@true

# Build every fixture both ways and check the two back-ends emit the
# same instructions.
compare: compare.rb
	@for f in $(FIXTURES); do ./compare.rb $$f bin $(BINFORMAT) || exit 1; done

lt: test.rb test_lt.code
	@./test.rb lt bin $(BINFORMAT) $(FORMAT)

gt: test.rb test_gt.code
	@./test.rb gt bin $(BINFORMAT) $(FORMAT)

ge: test.rb test_ge.code
	@./test.rb ge bin $(BINFORMAT) $(FORMAT)

le: test.rb test_le.code
	@./test.rb le bin $(BINFORMAT) $(FORMAT)

eq: test.rb test_eq.code
	@./test.rb eq bin $(BINFORMAT) $(FORMAT)

neq: test.rb test_neq.code
	@./test.rb neq bin $(BINFORMAT) $(FORMAT)

if: test.rb test_if.code
	@./test.rb if bin $(BINFORMAT) $(FORMAT)

while: test.rb test_while.code
	@./test.rb while bin $(BINFORMAT) $(FORMAT)

until: test.rb test_until.code
	@./test.rb until bin $(BINFORMAT) $(FORMAT)

repeat: test.rb test_repeat.code
	@./test.rb repeat bin $(BINFORMAT) $(FORMAT)

for: test.rb test_for.code
	@./test.rb for bin $(BINFORMAT) $(FORMAT)

do: test.rb test_do.code
	@./test.rb do bin $(BINFORMAT) $(FORMAT)

break: test.rb test_break.code
	@./test.rb break bin $(BINFORMAT) $(FORMAT)

print: test.rb test_print.code
	@./test.rb print bin $(BINFORMAT) $(FORMAT)

big: test.rb test_big.code
	@./test.rb big bin $(BINFORMAT) $(FORMAT)

huge: test.rb test_huge.code
	@./test.rb huge bin $(BINFORMAT) $(FORMAT)

or: test.rb test_or.code
	@./test.rb or bin $(BINFORMAT) $(FORMAT)

and: test.rb test_and.code
	@./test.rb and bin $(BINFORMAT) $(FORMAT)

bit_ops: test.rb test_bit_ops.code
	@./test.rb bit_ops bin $(BINFORMAT) $(FORMAT)

clean:
	@rm -f bin/*

.PHONY: all compare clean