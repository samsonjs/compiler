GLOBAL _start
SECTION .data

SECTION .bss
DIGITS: resd 4
HEX: resd 3
xitcode: resd 1

SECTION .text
_start:
	mov dword [DIGITS], 0x33323130
	mov dword [DIGITS+4], 0x37363534
	mov dword [DIGITS+8], 0x62613938
	mov dword [DIGITS+12], 0x66656463
	mov word [HEX], 0x7830
	mov word [HEX+10], 0xa
	mov eax, 0
	lea esi, [DIGITS]
	lea edi, [HEX+9]
	mov ecx, 0x4
L000001:
	movzx ebx, al
	and bl, 15
	movzx edx, byte [esi+ebx]
	mov byte [edi], dl
	dec edi
	movzx ebx, al
	and bl, 240
	shr bl, 4
	mov dl, byte [esi+ebx]
	mov byte [edi], dl
	dec edi
	shr eax, 8
	loop L000001
	mov eax, 0x4
	mov ebx, 0x1
	lea ecx, [HEX]
	mov edx, 0xb
	int 0x80
	mov word [HEX], 0x7830
	mov word [HEX+10], 0xa
	mov eax, 1
	lea esi, [DIGITS]
	lea edi, [HEX+9]
	mov ecx, 0x4
L000002:
	movzx ebx, al
	and bl, 15
	movzx edx, byte [esi+ebx]
	mov byte [edi], dl
	dec edi
	movzx ebx, al
	and bl, 240
	shr bl, 4
	mov dl, byte [esi+ebx]
	mov byte [edi], dl
	dec edi
	shr eax, 8
	loop L000002
	mov eax, 0x4
	mov ebx, 0x1
	lea ecx, [HEX]
	mov edx, 0xb
	int 0x80
	mov word [HEX], 0x7830
	mov word [HEX+10], 0xa
	mov eax, 1
	neg eax
	lea esi, [DIGITS]
	lea edi, [HEX+9]
	mov ecx, 0x4
L000003:
	movzx ebx, al
	and bl, 15
	movzx edx, byte [esi+ebx]
	mov byte [edi], dl
	dec edi
	movzx ebx, al
	and bl, 240
	shr bl, 4
	mov dl, byte [esi+ebx]
	mov byte [edi], dl
	dec edi
	shr eax, 8
	loop L000003
	mov eax, 0x4
	mov ebx, 0x1
	lea ecx, [HEX]
	mov edx, 0xb
	int 0x80
	mov word [HEX], 0x7830
	mov word [HEX+10], 0xa
	mov eax, 123
	lea esi, [DIGITS]
	lea edi, [HEX+9]
	mov ecx, 0x4
L000004:
	movzx ebx, al
	and bl, 15
	movzx edx, byte [esi+ebx]
	mov byte [edi], dl
	dec edi
	movzx ebx, al
	and bl, 240
	shr bl, 4
	mov dl, byte [esi+ebx]
	mov byte [edi], dl
	dec edi
	shr eax, 8
	loop L000004
	mov eax, 0x4
	mov ebx, 0x1
	lea ecx, [HEX]
	mov edx, 0xb
	int 0x80
	mov word [HEX], 0x7830
	mov word [HEX+10], 0xa
	mov eax, 123
	neg eax
	lea esi, [DIGITS]
	lea edi, [HEX+9]
	mov ecx, 0x4
L000005:
	movzx ebx, al
	and bl, 15
	movzx edx, byte [esi+ebx]
	mov byte [edi], dl
	dec edi
	movzx ebx, al
	and bl, 240
	shr bl, 4
	mov dl, byte [esi+ebx]
	mov byte [edi], dl
	dec edi
	shr eax, 8
	loop L000005
	mov eax, 0x4
	mov ebx, 0x1
	lea ecx, [HEX]
	mov edx, 0xb
	int 0x80
	mov word [HEX], 0x7830
	mov word [HEX+10], 0xa
	mov eax, 4096
	lea esi, [DIGITS]
	lea edi, [HEX+9]
	mov ecx, 0x4
L000006:
	movzx ebx, al
	and bl, 15
	movzx edx, byte [esi+ebx]
	mov byte [edi], dl
	dec edi
	movzx ebx, al
	and bl, 240
	shr bl, 4
	mov dl, byte [esi+ebx]
	mov byte [edi], dl
	dec edi
	shr eax, 8
	loop L000006
	mov eax, 0x4
	mov ebx, 0x1
	lea ecx, [HEX]
	mov edx, 0xb
	int 0x80
	mov word [HEX], 0x7830
	mov word [HEX+10], 0xa
	mov eax, 4096
	neg eax
	lea esi, [DIGITS]
	lea edi, [HEX+9]
	mov ecx, 0x4
L000007:
	movzx ebx, al
	and bl, 15
	movzx edx, byte [esi+ebx]
	mov byte [edi], dl
	dec edi
	movzx ebx, al
	and bl, 240
	shr bl, 4
	mov dl, byte [esi+ebx]
	mov byte [edi], dl
	dec edi
	shr eax, 8
	loop L000007
	mov eax, 0x4
	mov ebx, 0x1
	lea ecx, [HEX]
	mov edx, 0xb
	int 0x80
	mov eax, 0
	mov dword [xitcode], eax

	;; The result in eax is the exit code, move it to ebx.
	mov ebx, eax
	mov eax, 1		; _exit syscall
	int 0x80		; call Linux
