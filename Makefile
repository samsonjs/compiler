test:
	cd test && make all

elfwriter: elfwriter.c
	gcc -o elfwriter elfwriter.c -lelf

test_elf: elfwriter build
	./elfwriter test.bin 4 test_elf.o
	ld -o test_elf test_elf.o
	./test_elf

clean:
	@rm -f elfwriter
	@rm -f test_elf.o
	@rm -f test_elf

.PHONY: test
