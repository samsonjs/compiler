require 'asm/cstruct'

# The ELF module contains constants and structures for 32-bit ELF
# object files, as defined in <elf.h>.  Only what is needed to write
# a relocatable i386 object is here.

module ELF


  ##############
  # ELF header #
  ##############

  class Header < CStruct
    string :e_ident, 16
    uint16 :e_type
    uint16 :e_machine
    uint32 :e_version
    uint32 :e_entry
    uint32 :e_phoff
    uint32 :e_shoff
    uint32 :e_flags
    uint16 :e_ehsize
    uint16 :e_phentsize
    uint16 :e_phnum
    uint16 :e_shentsize
    uint16 :e_shnum
    uint16 :e_shstrndx
  end

  ELFMAG       = "\x7fELF"
  ELFCLASS32   = 1
  ELFDATA2LSB  = 1
  EV_CURRENT   = 1

  # Values for e_type.
  ET_REL  = 1
  ET_EXEC = 2

  # Values for e_machine.
  EM_386 = 3


  ###################
  # Section headers #
  ###################

  class SectionHeader < CStruct
    uint32 :sh_name      # Index into the section header string table.
    uint32 :sh_type
    uint32 :sh_flags
    uint32 :sh_addr
    uint32 :sh_offset
    uint32 :sh_size
    uint32 :sh_link
    uint32 :sh_info
    uint32 :sh_addralign
    uint32 :sh_entsize
  end

  # Values for sh_type.
  SHT_NULL     = 0
  SHT_PROGBITS = 1
  SHT_SYMTAB   = 2
  SHT_STRTAB   = 3
  SHT_NOBITS   = 8
  SHT_REL      = 9

  # Values for sh_flags.
  SHF_WRITE     = 0x1
  SHF_ALLOC     = 0x2
  SHF_EXECINSTR = 0x4


  ################
  # Symbol table #
  ################

  class Sym < CStruct
    uint32 :st_name      # Index into the string table.
    uint32 :st_value
    uint32 :st_size
    uint8  :st_info      # Binding in the high nybble, type in the low one.
    uint8  :st_other
    uint16 :st_shndx     # Section the symbol is defined in.
  end

  # Symbol bindings.
  STB_LOCAL  = 0
  STB_GLOBAL = 1

  # Symbol types.
  STT_NOTYPE  = 0
  STT_OBJECT  = 1
  STT_FUNC    = 2
  STT_SECTION = 3

  def self.st_info(bind, type)
    (bind << 4) | (type & 0xf)
  end


  ###############
  # Relocations #
  ###############

  class Rel < CStruct
    uint32 :r_offset     # Offset in the section of the value to relocate.
    uint32 :r_info       # Symbol index in the high 24 bits, type in the low 8.
  end

  # Relocation types (i386).
  R_386_32 = 1         # S + A, where A is the value already in place.

  def self.r_info(sym, type)
    (sym << 8) | (type & 0xff)
  end

end
