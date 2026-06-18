require 'compiler/cstruct'

# The MachO module contains constants and structures related to the
# Mach Object format (Mach-O). They are relevant to Darwin on OS X.
#
# Constants and structures as defined in /usr/include/mach-o/loader.h
# on Mac OS X Leopard (10.5.7). Also see <mach-o/stab.h>,
# <mach-o/nlist.h>, and <mach-o/reloc.h>.

class Compiler
  module MachO

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

  end
end
