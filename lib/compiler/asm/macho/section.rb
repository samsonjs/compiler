require 'compiler/cstruct'

# The MachO module contains constants and structures related to the
# Mach Object format (Mach-O). They are relevant to Darwin on OS X.
#
# Constants and structures as defined in /usr/include/mach-o/loader.h
# on Mac OS X Leopard (10.5.7). Also see <mach-o/stab.h>,
# <mach-o/nlist.h>, and <mach-o/reloc.h>.

class Compiler
  module MachO

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

  end
end
