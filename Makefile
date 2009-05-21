lt: test.rb test_lt.code
	ruby test.rb test_lt.code > test_lt.asm
	nasm -f elf -g -o test_lt.o test_lt.asm
	ld -o test_lt test_lt.o
# $? indicates success as per unix convention
	./test_lt

print: test.rb test_print.code
	ruby test.rb test_print.code > test_print.asm
	nasm -f elf -g -o test_print.o test_print.asm
	ld -o test_print test_print.o
# $? indicates success as per unix convention
	./test_print

build: test.rb test.code
	ruby test.rb test.code > test.asm
	nasm -f elf -g -o test.o test.asm
	ld -o test test.o
# $? indicates success as per unix convention
	./test

clean:
	@rm -f test.o
	@rm -f test
	@rm -f test.asm

