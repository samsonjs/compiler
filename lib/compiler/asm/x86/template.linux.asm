BITS 32

SECTION .data
{data}

SECTION .bss
{bss}

SECTION .text
GLOBAL _start
_start:
{code}
	; exit(eax)
	mov ebx, eax
	mov eax, 1
	int 0x80
