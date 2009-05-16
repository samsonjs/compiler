build:
	ruby test.rb test.code
	nasm -f elf -g -o test.o test.asm
	ld -o test test.o
# $? indicates success as per unix convention
	./test

clean:
	@rm -f test.o
	@rm -f test
	@rm -f test.asm

