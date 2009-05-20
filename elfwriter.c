#include <libelf.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

/* _exit(0) */
/* uint8_t shell_code[] = { */
/*         0xbb, 0,    0,    0,    0, /\* mov ebx, 0 *\/ */
/*         0xb8, 1,    0,    0,    0, /\* mov eax, 1 *\/ */
/*         0xcd, 0x80                 /\* int 0x80 *\/ */
/* }; */

/* uint32_t hash_words[] = { */
/*         0x12345678, */
/*         0xdeadc0de, */
/*         0x1234abcd */
/* }; */

#define header_size 0x100
#define text_addr 0x8048000 + header_size
#define text_size 0x02be00
#define data_addr text_addr + text_size
#define data_size 0x4e00
#define bss_addr data_addr + data_size
size_t bss_size = 0;

char string_table[] = {
        /* Offset  0 */ '\0',
        /* Offset  1 */ '.', 't', 'e', 'x', 't', '\0' ,
        /* Offset  7 */ '.', 'b', 's', 's', '\0',
        /* Offset 12 */ '.', 's', 'h', 's', 't', 'r', 't', 'a', 'b', '\0'
};


/* Write a static 32-bit x86 ELF binary to filename.  The file is
 * clobbered without confirmation!
 */
int
elf_write(const char *filename, uint8_t *code, size_t code_size)
{
        int fd;
        size_t shstrndx;
        Elf *elf;
        Elf_Scn *scn;
        Elf_Data *data;
        Elf32_Ehdr *ehdr;
        Elf32_Phdr *phdr, *load;
        Elf32_Shdr *shdr;

        if (elf_version(EV_CURRENT) == EV_NONE) {
                printf("Failed to initialize ELF library!\n");
                return -1;
        }
        if ((fd = open(filename, O_RDWR|O_TRUNC|O_CREAT, 0666)) < 0) {
                printf("Can't open %s for writing.\n", filename);
                perror("[elf_write]");
                return -2;
        }
        if ((elf = elf_begin(fd, ELF_C_WRITE, (Elf *)0)) == 0) {
                printf("elf_begin failed!\n");
                return -3;
        }

        
        /**************
         * ELF Header *
         **************/
        
        if ((ehdr = elf32_newehdr(elf)) == NULL) {
                printf("elf32_newehdr failed!\n");
                return -4;
        }
        ehdr->e_ident[EI_DATA] = ELFDATA2LSB; /* 2's complement, little endian */
        ehdr->e_type = ET_EXEC;
        ehdr->e_machine = EM_386; /* x86 */
        
        /* Image starts at 0x8048000, x86 32-bit abi.  We need a bit
         * of room for headers and such.  TODO figure out how much
         * room is needed!
         *
         * Current entry point is .text section.
         */
        ehdr->e_entry = text_addr;
        

        /*******************
         * Program Headers *
         *******************/
        
        if ((phdr = elf32_newphdr(elf, 2)) == NULL) {
                printf("elf32_newphdr failed!\n");
                return -5;
        }
        load = phdr+1;


        /*****************
         * .text section *
         *****************/
        
        if ((scn = elf_newscn(elf)) == NULL) {
                printf("elf_newscn failed!\n");
                return -6;
        }
        if ((data = elf_newdata(scn)) == NULL) {
                printf("elf_newdata failed!\n");
                return -7;
        }
        data->d_align = 16;
        data->d_buf = code;
        data->d_off = 0LL;
        data->d_type = ELF_T_BYTE;
        data->d_size = code_size;
        data->d_version = EV_CURRENT;
        
        if ((shdr = elf32_getshdr(scn)) == NULL) {
                printf("elf32_getshdr failed!\n");
                return -8;
        }
        shdr->sh_name = 1;
        shdr->sh_type = SHT_PROGBITS;
        shdr->sh_flags = SHF_EXECINSTR | SHF_ALLOC;
        shdr->sh_addr = text_addr;

        
        /****************
         * .bss section *
         ****************/

        if ((scn = elf_newscn(elf)) == NULL) {
                printf("elf_newscn failed!\n");
                return -6;
        }
        if ((data = elf_newdata(scn)) == NULL) {
                printf("elf_newdata failed!\n");
                return -7;
        }
        data->d_align = 4;
        data->d_off = 0LL;
        data->d_type = ELF_T_BYTE;
        data->d_size = bss_size;
        data->d_version = EV_CURRENT;
        
        if ((shdr = elf32_getshdr(scn)) == NULL) {
                printf("elf32_getshdr failed!\n");
                return -8;
        }
        shdr->sh_name = 7;
        shdr->sh_type = SHT_NOBITS;
        shdr->sh_flags = SHF_WRITE | SHF_ALLOC;
        shdr->sh_addr = bss_addr;

        
        /*******************************
         * section header string table *
         *******************************/

        if ((scn = elf_newscn(elf)) == NULL) {
                printf("elf_newscn failed!\n");
                return -9;
        }
        if ((data = elf_newdata(scn)) == NULL) {
                printf("elf_newdata failed!\n");
                return -10;
        }
        data->d_align = 1;
        data->d_buf = string_table;
        data->d_off = 0LL;
        data->d_type = ELF_T_BYTE;
        data->d_size = sizeof(string_table);
        data->d_version = EV_CURRENT;

        if ((shdr = elf32_getshdr(scn)) == NULL) {
                printf("elf32_getshdr failed!\n");
                return -11;
        }
        shdr->sh_name = 12;
        shdr->sh_type = SHT_STRTAB;
        shdr->sh_flags = SHF_STRINGS | SHF_ALLOC;
        shdr->sh_entsize  = 0;

        
        /* int elf_setshstrndx(Elf *e, Elf32_Ehdr *eh, size_t shstrndx) */
        shstrndx = elf_ndxscn(scn);
        if (shstrndx >= SHN_LORESERVE) {
                if ((scn = elf_getscn(elf, 0)) == NULL) {
                        printf("elf_getscn failed!\n");
                        return -12;
                }
                /* assert(scn->s_ndx == SHN_UNDEF); */
                /* scn->s_shdr.s_shdr32.sh_link = shstrndx; */
                elf_flagshdr(scn, ELF_C_SET, ELF_F_DIRTY);
                shstrndx = SHN_XINDEX;
        }
        ehdr->e_shstrndx = shstrndx;
        
        if (elf_update(elf, ELF_C_NULL) < 0) {
                printf("elf_update failed!\n");
                return -12;
        }

        phdr->p_vaddr = phdr->p_paddr = 0x8048000 + ehdr->e_phoff;
        phdr->p_type = PT_PHDR;
        phdr->p_offset = ehdr->e_phoff;
        phdr->p_filesz = elf32_fsize(ELF_T_PHDR, 1, EV_CURRENT);

        load->p_vaddr = phdr->p_paddr = 0x8048000;
        load->p_type = PT_LOAD;
        load->p_offset = 0;
        load->p_filesz = elf32_fsize(ELF_T_PHDR, 1, EV_CURRENT);
        load->p_flags = PF_R | PF_X;
        load->p_align = 0x1000;

        elf_flagphdr(elf, ELF_C_SET, ELF_F_DIRTY);
        
        if (elf_update(elf, ELF_C_WRITE) < 0) {
                printf("elf_update failed!\n");
                return -13;
        }
        
        elf_end(elf);
        close(fd);
        return 0;
}

int
main(int argc, const char *argv[])
{
        int result;
        pid_t pid;
        FILE *fd;
        uint8_t *code = NULL;
        size_t code_size = 0, chunk_size = 1024, bytes_read;

        if (argc < 4) {
                printf("usage: %s <input> <bss_size> <output>\n", argv[0]);
                printf("  Wraps the input file in an ELF binary.\n");
                return 1;
        }

        bss_size = strtoul(argv[2], 0, 10);
        
        if ((fd = fopen(argv[1], "r")) < 0) {
                printf("[error] can't open %s for reading.\n", argv[1]);
                perror("[main]");
                return 2;
        }
        while (!feof(fd) && !ferror(fd)) {
                code = realloc(code, code_size + chunk_size);
                bytes_read = fread(code+code_size, 1, chunk_size, fd);
                code_size += bytes_read;
        }
        fclose(fd);
        
        printf("Writing x86 ELF binary to %s...\n", argv[1]);
        result = elf_write(argv[3], code, code_size);
        if (result < 0) {
                printf("[error] elf_write failed.\n");
                return 3;
        }

        return 0;
}
