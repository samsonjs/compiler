require 'compiler/cstruct'

# The MachO module contains constants and structures related to the
# Mach Object format (Mach-O). They are relevant to Darwin on OS X.
#
# Constants and structures as defined in /usr/include/mach-o/loader.h
# on Mac OS X Leopard (10.5.7). Also see <mach-o/stab.h>,
# <mach-o/nlist.h>, and <mach-o/reloc.h>.

class Compiler
  module MachO

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

  end
end
