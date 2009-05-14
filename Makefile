build:
#	ruby test.rb '5-5+3-2-1'
	ruby test.rb '5*3-5*2+3-9/3-1*1-8/2'
	nasm -f elf -g -o test.o test.asm
	ld -o test test.o
# $? indicates success as per unix convention
	./test

