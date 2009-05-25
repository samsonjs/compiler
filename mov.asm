BITS 32

;;; 00000000  b8 78 56 34 12 b9 78 56  34 12 ba 78 56 34 12 bb  |.xV4..xV4..xV4..|
;;; 00000010  78 56 34 12 89 c0 89 c8  89 d0 89 d8 89 c1 89 c9  |xV4.............|
;;; 00000020  89 d1 89 d9 89 c2 89 ca  89 d2 89 da 89 c3 89 cb  |................|
;;; 00000030  89 d3 89 db a1 ef be ad  de 8b 0d ef be ad de 8b  |................|
;;; 00000040  15 ef be ad de 8b 1d ef  be ad de a3 ef be ad de  |................|
;;; 00000050  89 0d ef be ad de 89 15  ef be ad de 89 1d ef be  |................|
;;; 00000060  ad de 8b 00 8b 01 8b 02  8b 03 8b 08 8b 09 8b 0a  |................|
;;; 00000070  8b 0b 8b 10 8b 11 8b 12  8b 13 8b 18 8b 19 8b 1a  |................|
;;; 00000080  8b 1b 89 00 89 01 89 02  89 03 89 08 89 09 89 0a  |................|
;;; 00000090  89 0b 89 10 89 11 89 12  89 13 89 18 89 19 89 1a  |................|
;;; 000000a0  89 1b                                             |..|
;;; 000000a2

mov eax, 0x12345678		; b8 78 56 34 12
mov ecx, 0x12345678		; b9 78 56 34 12
mov edx, 0x12345678		; ba 78 56 34 12
mov ebx, 0x12345678		; bb 78 56 34 12

mov eax, eax			; 89 c0
mov eax, ecx			; 89 c8
mov eax, edx			; 89 d0
mov eax, ebx			; 89 d8

mov ecx, eax			; 89 c1
mov ecx, ecx			; 89 c9
mov ecx, edx			; 89 d1
mov ecx, ebx			; 89 d9

mov edx, eax			; 89 c2
mov edx, ecx			; 89 ca
mov edx, edx			; 89 d2
mov edx, ebx			; 89 da

mov ebx, eax			; 89 c3
mov ebx, ecx			; 89 cb
mov ebx, edx			; 89 d3
mov ebx, ebx			; 89 db

mov eax, dword [0xdeadbeef]	; a1 ef be ad de
mov ecx, dword [0xdeadbeef]	; 8b 0e ef be ad de
mov edx, dword [0xdeadbeef]	; 8b 16 ef be ad de
mov ebx, dword [0xdeadbeef]	; 8b 1e ef be ad de

mov [0xdeadbeef], eax		; a3 ef be ad de
mov [0xdeadbeef], ecx		; 89 0e ef be ad de
mov [0xdeadbeef], edx		; 89 16 ef be ad de
mov [0xdeadbeef], ebx		; 89 1e ef be ad de

mov eax, dword [eax]		; 8b 00
mov eax, dword [ecx]		; 8b 01
mov eax, dword [edx]		; 8b 02
mov eax, dword [ebx]		; 8b 03

mov ecx, dword [eax]		; 8b 08
mov ecx, dword [ecx]		; 8b 09
mov ecx, dword [edx]		; 8b 0a
mov ecx, dword [ebx]		; 8b 0b

mov edx, dword [eax]		; 8b 10
mov edx, dword [ecx]		; 8b 11
mov edx, dword [edx]		; 8b 12
mov edx, dword [ebx]		; 8b 13

mov ebx, dword [eax]		; 8b 18
mov ebx, dword [ecx]		; 8b 19
mov ebx, dword [edx]		; 8b 1a
mov ebx, dword [ebx]		; 8b 1b

mov [eax], eax			; 89 00
mov [ecx], eax			; 89 01
mov [edx], eax			; 89 02
mov [ebx], eax			; 89 03

mov [eax], ecx			; 89 08
mov [ecx], ecx			; 89 09
mov [edx], ecx			; 89 0a
mov [ebx], ecx			; 89 0b

mov [eax], edx			; 89 10
mov [ecx], edx			; 89 11
mov [edx], edx			; 89 12
mov [ebx], edx			; 89 13

mov [eax], ebx			; 89 18
mov [ecx], ebx			; 89 19
mov [edx], ebx			; 89 1a
mov [ebx], ebx			; 89 1b
