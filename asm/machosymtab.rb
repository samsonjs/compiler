require 'asm/macho'
require 'asm/machosym'
require 'asm/symtab'

module Assembler
  
  class MachOSymtab < Symtab
    
    include MachO

    def make_symbols(vars, base_addr, type, segnum)
      # Note: Sorting a Ruby hash gives an alist, e.g. [[<key>, <value>], ...]
      #       We can use map on it as if it were a hash so it works nicely.
      vars.sort { |a,b| a[1] <=> b[1] }.
           map do |name, offset|
             MachOSym.new(name, type, segnum, 0, base_addr + offset)
           end
    end

    def all_symbols
      # TODO FIXME:
      # - the last var exported ends up after main somewhere... WTF?!
      # - All labels are exported.  This should be changed and only functions exported!

      section = 1
      
      # Functions (section #1, __text)
      symbols = make_symbols(@labels, text_offset, N_SECT | N_EXT, section)
      section += 1

      # Constants (section #2, __const)
      if @consts.size > 0
        symbols += make_symbols(@consts, const_offset, N_SECT, section)
        section += 1
      end

      # Variables (section #3, __bss)
      if @vars.size > 0
        symbols += make_symbols(@vars, bss_offset, N_SECT, section)
      end

      return symbols
    end

    # this is fairly stupid but works
    def bss_section
      @consts.size > 0 ? 3 : 2
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
      # The empty strings result in a string that begins and ends with a null byte
      ['', all_symbols, ''].flatten.map { |sym| sym.to_s }.join("\0")
    end

    def reloc(r_address, r_symbolnum=0, r_length=2, r_extern=0, r_pcrel=0, r_type=0)
      r_info = (r_type << 28) | (r_extern << 27) | (r_length << 25) |
        (r_pcrel << 24) | r_symbolnum
      @reloc_info << RelocationInfo.new(r_address, r_info)
    end

    def reloc_info
      n = bss_section
      @reloc_info.each {|r| r[:r_info] |= n}
    end

    def calculate_offsets(text_size)
      @const_offset = @text_offset + text_size
      @bss_offset = @const_offset + @const_size
    end

  end
  
end
