require 'asm/cstruct'

# The MachO module contains constants and structures related to the
# Mach Object format (Mach-O).  They are relevant to Darwin on OS X.
#
# Constants and structures as defined in /usr/include/mach-o/loader.h
# on Mac OS X Leopard (10.5.7).  Also see <mach-o/stab.h>,
# <mach-o/nlist.h>, and <mach-o/reloc.h>.

module MachO


  ###############
  # Mach header #
  ###############

  # Appears at the beginning of every Mach object file.
  class MachHeader < CStruct
    uint32 :magic
    int32  :cputype
    int32  :cpusubtype
    uint32 :filetype
    uint32 :ncmds
    uint32 :sizeofcmds
    uint32 :flags
  end

  # Values for the magic field.
  MH_MAGIC = 0xfeedface          # Mach magic number (big-endian).
  MH_CIGAM = 0xcefaedfe          # Little-endian version.

  # Values for the filetype field.
  MH_OBJECT     = 0x1
  MH_EXECUTE    = 0x2
  MH_FVMLIB     = 0x3
  MH_CORE       = 0x4
  MH_PRELOAD    = 0x5
  MH_DYLIB      = 0x6
  MH_DYLINKER   = 0x7
  MH_BUNDLE     = 0x8
  MH_DYLIB_STUB = 0x9
  MH_DSYM       = 0xa

  # CPU types and subtypes (only Intel for now).
  CPU_TYPE_X86 = 7
  CPU_TYPE_I386 = CPU_TYPE_X86
  CPU_SUBTYPE_X86_ALL = 3


  ############################
  # Load commands / segments #
  ############################

  class LoadCommand < CStruct
    uint32 :cmd
    uint32 :cmdsize
  end

  # Values for the cmd member of LoadCommand CStructs (incomplete!).
  LC_SEGMENT        = 0x1
  LC_SYMTAB         = 0x2
  LC_SYMSEG         = 0x3
  LC_THREAD         = 0x4
  LC_UNIXTHREAD	    = 0x5

  class SegmentCommand < LoadCommand
    string :segname, 16
    uint32 :vmaddr
    uint32 :vmsize
    uint32 :fileoff
    uint32 :filesize
    int32  :maxprot
    int32  :initprot
    uint32 :nsects
    uint32 :flags
  end


  # Values for protection fields, maxprot and initprot.
  VM_PROT_NONE       = 0x00
  VM_PROT_READ       = 0x01
  VM_PROT_WRITE      = 0x02
  VM_PROT_EXECUTE    = 0x04
  VM_PROT_NO_CHANGE  = 0x08
  VM_PROT_COPY       = 0x10


  class SymtabCommand < LoadCommand
    uint32 :symoff     # Points to an array of Nlist structs.
    uint32 :nsyms      # Number of entries in said array.
    uint32 :stroff     # Offset of the string table.
    uint32 :strsize    # Size of the string table in bytes.
  end


  LoadCommandStructMap = {
    LC_SEGMENT => SegmentCommand,
    LC_SYMTAB  => SymtabCommand
  }


  ############
  # Sections #
  ############

  class Section < CStruct
    string :sectname, 16
    string :segname, 16
    uint32 :addr
    uint32 :size
    uint32 :offset
    uint32 :align
    uint32 :reloff
    uint32 :nreloc
    uint32 :flags
    uint32 :reserved1
    uint32 :reserved2
  end

  # Values for the type bitfield (mask 0x000000ff) of the flags field.
  # (incomplete!)
  S_REGULAR  = 0x0
  S_ZEROFILL = 0x1
  S_CSTRING_LITERALS = 0x2


  ###########################
  # Relocation info support #
  ###########################

  class RelocationInfo < CStruct
    int32  :r_address   # offset in the section to what is being relocated
    uint32 :r_info
  end

# NOTE: r_info is a packed bit field with the following members:
#
# (CStruct should eventually support bitfields, but doesn't right now.)
#
#     r_symbolnum : 24 -- symbol index if r_extern == 1 or section ordinal if r_extern == 0
#     r_pcrel     :  1 -- was relocated pc relative already
#     r_length    :  2 -- 0=byte, 1=word, 2=long, 3=quad
#     r_extern    :  1 -- 1 for exported symbols, 0 othewise
#     r_type      :  4 -- if not 0, machine specific relocation type (always 0)

  R_ABS = 0         # Absolute relocation type
                    # (r_symbolnum == R_ABS for absolute symbols that don't need reloc)

  # Relocation types (r_type)
  GENERIC_RELOC_VANILLA = 0


  ########################
  # Symbol table support #
  ########################


  # Nlist is used to describe symbols.
  class Nlist < CStruct
    uint32 :n_strx     # Index into string table. Index of zero is the empty string.
    uint8  :n_type     # Type flag (see below).
    uint8  :n_sect     # Section number (from 1) or NO_SECT.
    uint16 :n_desc     # TODO See <mach-o/stab.h>.
    uint32 :n_value    # The symbol's value (or stab offset).
  end

  # Type flag (see <mach-o/nlist.h> for more details)
  # ---------
  #
  # This field consists of four bitfields:
  #
  #   uchar N_STAB : 3
  #   uchar N_PEXT : 1
  #   uchar N_TYPE : 3
  #   uchar N_EXT  : 1
  #
  N_STAB = 0xe0   # if any bits set => symbolic debugging info
  N_PEXT = 0x10   # private external symbol bit
  N_TYPE = 0x0e   # mask for the type bits
  N_EXT  = 0x01   # external symbol bit, set for external symbols (e.g. globals)

  # Values for N_TYPE. (incomplete!)
  N_UNDF = 0x0    # undefined, n_sect == NO_SECT
  N_ABS  = 0x2    # absolute, n_sect == NO_SECT
  N_SECT = 0xe    # defined in section number n_sect

  NO_SECT = 0
  MAX_SECT = 255

end
