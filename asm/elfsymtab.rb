require "asm/elf"
require "asm/symtab"

module Assembler
  class ELFSymtab < Symtab
    ELFSymbol = Data.define(:name, :value, :section, :type, :bind)

    # Symbols are offsets into their own sections and the linker adds
    # each section's final address, so nothing moves here.
    def calculate_offsets(text_size)
      @const_offset = 0
      @bss_offset = 0
    end

    # Record an offset in the text section holding an address that
    # needs relocating.  The compiler only takes the address of
    # variables so they are all relocated against .bss, like Mach-O.
    def reloc(offset)
      @reloc_info << offset
    end

    def local_symbols
      labels = @labels.sort_by { |_, offset| offset }.map do |name, offset|
        ELFSymbol.new(name:, value: offset, section: :text, type: ELF::STT_NOTYPE,
          bind: ELF::STB_LOCAL)
      end
      consts = @consts.sort_by { |_, offset| offset }.map do |name, offset|
        ELFSymbol.new(name:, value: offset, section: :const, type: ELF::STT_OBJECT,
          bind: ELF::STB_LOCAL)
      end
      vars = @vars.sort_by { |_, offset| offset }.map do |name, offset|
        ELFSymbol.new(name:, value: offset, section: :bss, type: ELF::STT_OBJECT,
          bind: ELF::STB_LOCAL)
      end
      labels + consts + vars
    end

    # ld's default entry point.
    def global_symbols
      [ELFSymbol.new(name: "_start", value: lookup_label("_main"), section: :text,
        type: ELF::STT_FUNC, bind: ELF::STB_GLOBAL)]
    end
  end
end
