BITS 32
GLOBAL _start
SECTION .data
SECTION .bss
SECTION .text
_start:
	mov ebx, 42
	mov eax, 1		; _exit syscall
	int 0x80		; call Linux
