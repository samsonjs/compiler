GLOBAL _start
SECTION .data
{data}
SECTION .bss
{bss}
SECTION .text
_start:
{code}
	;; The result in eax is the exit code, move it to ebx.
	mov ebx, eax
	mov eax, 1		; _exit syscall
	int 0x80		; call Linux
