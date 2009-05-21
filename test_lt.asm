GLOBAL _start
SECTION .data

SECTION .bss
z: resd 1
x: resd 1
y: resd 1

SECTION .text
_start:
	mov eax, 1
	push eax
	mov eax, 1
	pop ebx
	cmp ebx, eax
	jl L000001_lt_1
	xor eax, eax
	jmp L000002_endlt_1
L000001_lt_1:
	xor eax, eax
	not eax
L000002_endlt_1:
	mov dword [z], eax
	mov eax, 1
	push eax
	mov eax, 3
	pop ebx
	cmp ebx, eax
	jl L000003_lt_2
	xor eax, eax
	jmp L000004_endlt_2
L000003_lt_2:
	xor eax, eax
	not eax
L000004_endlt_2:
	mov dword [x], eax
	mov eax, 3
	push eax
	mov eax, 1
	pop ebx
	cmp ebx, eax
	jl L000005_lt_3
	xor eax, eax
	jmp L000006_endlt_3
L000005_lt_3:
	xor eax, eax
	not eax
L000006_endlt_3:
	mov dword [y], eax

	;; The result in eax is the exit code, move it to ebx.
	mov ebx, eax
	mov eax, 1		; _exit syscall
	int 0x80		; call Linux
