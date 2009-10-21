require 'asm/macho'
require 'asm/machosym'
require 'asm/symtab'

module Assembler
  
  class MachOSymtab < Symtab
    
    include MachO

    def const_offset
      return 0x2000
    end

    def bss_offset
      # TODO figure out how to calculate these, or how to let the linker do it!
      #      ... relocation tables perhaps?
      return 0x2800
    end

    def make_symbols(vars, type, segnum)
      # Note: Sorting a Ruby hash gives an alist, e.g. [[<key>, <value>], ...]
      #       We can use map on it as if it were a hash so it works nicely.
      vars.sort { |a,b| a[1] <=> b[1] }.
           map do |name, addr|
             MachOSym.new(name, type, segnum, 0, addr)
           end
    end

    def all_symbols
      # TODO FIXME:
      # - the last var exported ends up after main somewhere... WTF?!
      # - All labels are exported.  This should be changed and only functions exported!
      symbols = make_symbols(@labels, N_SECT | N_EXT, 1) + # Functions (section #1, __text)
                make_symbols(@consts, N_SECT, 2)         + # Constants (section #2, __const)
                make_symbols(@vars, N_SECT, 3)             # Variables (section #3, __bss)
      return symbols
    end
    
    def nlist_ary
      symbols = {}
      strx = 1
      ary = []
      all_symbols.each do |sym|
        key = sym.name.to_sym
        unless symbols.has_key?(key)
          symbols[key] = strx
          strx += sym.name.length + 1 # +1 for the null byte
        end
        ary << sym.to_nlist(symbols[key])
      end
      return ary
    end
    
    def stab
      # The empty strings result in a string that begins and ends with
      ['', all_symbols, ''].flatten.map { |sym| sym.to_s }.join("\0")
    end

  end
  
end
