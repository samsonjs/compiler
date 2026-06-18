require 'compiler/cstruct'

# The MachO module contains constants and structures related to the
# Mach Object format (Mach-O). They are relevant to Darwin on OS X.
#
# Constants and structures as defined in /usr/include/mach-o/loader.h
# on Mac OS X Leopard (10.5.7). Also see <mach-o/stab.h>,
# <mach-o/nlist.h>, and <mach-o/reloc.h>.

class Compiler
  module MachO

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


    class SymbolTableCommand < LoadCommand
      uint32 :symoff     # Points to an array of Nlist structs.
      uint32 :nsyms      # Number of entries in said array.
      uint32 :stroff     # Offset of the string table.
      uint32 :strsize    # Size of the string table in bytes.
    end


    LOAD_COMMAND_STRUCT_MAP = {
      LC_SEGMENT => SegmentCommand,
      LC_SYMTAB  => SymbolTableCommand
    }

  end
end
