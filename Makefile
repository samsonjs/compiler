build: test.rb test.code
	ruby test.rb test.code
	nasm -f elf -g -o test.o test.asm
	ld -o test test.o
# $? indicates success as per unix convention
	./test

elfwriter: elfwriter.c
	gcc -o elfwriter elfwriter.c -lelf

test_elf: elfwriter build
	./elfwriter test.bin 4 test_elf.o
	ld -o test_elf test_elf.o
	./test_elf

clean:
	@rm -f test.o
	@rm -f test
	@rm -f test.asm
	@rm -f elfwriter
	@rm -f test_elf.o
	@rm -f test_elf
