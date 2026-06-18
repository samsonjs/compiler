BITS 32
GLOBAL _main
SECTION .data
SECTION .bss
SECTION .text
_main:
        mov eax, 42
	;; The result in eax is the exit code, just return.
	ret
