BITS 32

lea eax, [ebx+ecx*4]
lea ebx, [eax+ecx*4]
lea eax, [ecx+ebx*4]
lea eax, [ecx+ebx*8]
lea eax, [ecx+ebx]
lea eax, [0x1000+10*4]
lea eax, [eax]
lea eax, [ecx]
lea ecx, [eax]
lea eax, [0xdeadbeef]
