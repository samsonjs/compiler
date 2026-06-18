require 'compiler/cstruct'

# The MachO module contains constants and structures related to the
# Mach Object format (Mach-O). They are relevant to Darwin on OS X.
#
# Constants and structures as defined in /usr/include/mach-o/loader.h
# on Mac OS X Leopard (10.5.7). Also see <mach-o/stab.h>,
# <mach-o/nlist.h>, and <mach-o/reloc.h>.

class Compiler
  module MachO

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
end
