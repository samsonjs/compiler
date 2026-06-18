BITS 32
GLOBAL _main
SECTION .data
{data}
SECTION .bss
{bss}
SECTION .text
_main:
{code}
	;; The result in eax is the exit code, just return.
	ret
