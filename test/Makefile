all: lt gt ge le eq neq if while until repeat for do break print
	@echo -n

lt: test.rb test_lt.code
	@./test.rb lt

gt: test.rb test_gt.code
	@./test.rb gt

ge: test.rb test_ge.code
	@./test.rb ge

le: test.rb test_le.code
	@./test.rb le

eq: test.rb test_eq.code
	@./test.rb eq

neq: test.rb test_neq.code
	@./test.rb neq

if: test.rb test_if.code
	@./test.rb if

while: test.rb test_while.code
	@./test.rb while

until: test.rb test_until.code
	@./test.rb until

repeat: test.rb test_repeat.code
	@./test.rb repeat

for: test.rb test_for.code
	@./test.rb for

do: test.rb test_do.code
	@./test.rb do

break: test.rb test_break.code
	@./test.rb break

print: test.rb test_print.code
	@./test.rb print

big_test: test.rb big_test.code
	@./test.rb big

clean:
	@rm -f test*.asm test*.o
	@rm -f test_big test_lt test_gt test_ge test_le test_eq test_neq
	@rm -f test_while test_if test_until test_repeat test_do
	@rm -f test_for test_break test_print

.PHONY: clean