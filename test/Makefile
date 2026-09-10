PLATFORM=$(shell uname -s)
# Build products land here.  Not "bin": that is where a repo keeps the
# executables it ships, and this is throwaway output.
OUTDIR=tmp
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

# Record what every fixture prints, to review as a diff.
bless: bless.rb
	@for f in $(FIXTURES); do ./bless.rb $$f $(OUTDIR) $(BINFORMAT) $(FORMAT) || exit 1; done

# Build every fixture both ways and check the two back-ends emit the
# same instructions.
compare: compare.rb
	@for f in $(FIXTURES); do ./compare.rb $$f $(OUTDIR) $(BINFORMAT) || exit 1; done

lt: test.rb test_lt.code
	@./test.rb lt $(OUTDIR) $(BINFORMAT) $(FORMAT)

gt: test.rb test_gt.code
	@./test.rb gt $(OUTDIR) $(BINFORMAT) $(FORMAT)

ge: test.rb test_ge.code
	@./test.rb ge $(OUTDIR) $(BINFORMAT) $(FORMAT)

le: test.rb test_le.code
	@./test.rb le $(OUTDIR) $(BINFORMAT) $(FORMAT)

eq: test.rb test_eq.code
	@./test.rb eq $(OUTDIR) $(BINFORMAT) $(FORMAT)

neq: test.rb test_neq.code
	@./test.rb neq $(OUTDIR) $(BINFORMAT) $(FORMAT)

if: test.rb test_if.code
	@./test.rb if $(OUTDIR) $(BINFORMAT) $(FORMAT)

while: test.rb test_while.code
	@./test.rb while $(OUTDIR) $(BINFORMAT) $(FORMAT)

until: test.rb test_until.code
	@./test.rb until $(OUTDIR) $(BINFORMAT) $(FORMAT)

repeat: test.rb test_repeat.code
	@./test.rb repeat $(OUTDIR) $(BINFORMAT) $(FORMAT)

for: test.rb test_for.code
	@./test.rb for $(OUTDIR) $(BINFORMAT) $(FORMAT)

do: test.rb test_do.code
	@./test.rb do $(OUTDIR) $(BINFORMAT) $(FORMAT)

break: test.rb test_break.code
	@./test.rb break $(OUTDIR) $(BINFORMAT) $(FORMAT)

print: test.rb test_print.code
	@./test.rb print $(OUTDIR) $(BINFORMAT) $(FORMAT)

big: test.rb test_big.code
	@./test.rb big $(OUTDIR) $(BINFORMAT) $(FORMAT)

huge: test.rb test_huge.code
	@./test.rb huge $(OUTDIR) $(BINFORMAT) $(FORMAT)

or: test.rb test_or.code
	@./test.rb or $(OUTDIR) $(BINFORMAT) $(FORMAT)

and: test.rb test_and.code
	@./test.rb and $(OUTDIR) $(BINFORMAT) $(FORMAT)

bit_ops: test.rb test_bit_ops.code
	@./test.rb bit_ops $(OUTDIR) $(BINFORMAT) $(FORMAT)

clean:
	@rm -rf $(OUTDIR)

.PHONY: all bless compare clean