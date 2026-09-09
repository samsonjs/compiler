require "asm/elf"
require "asm/objwriter"

module Assembler
  # Writes an ELF32 relocatable object for i386, much like nasm -f elf
  # does, for ld -m elf_i386 to turn into an executable.
  class ELFFile < ObjWriter
    include ELF

    # Names are looked up in a string table, hence name_index.
    Section = Data.define(:name_index, :type, :flags, :data, :size, :link, :info, :align,
      :entsize)

    class StringTable
      attr_reader :data

      def initialize
        @data = "\0"
        @index = {}
      end

      def add(name)
        @index[name] ||= begin
          index = @data.bytesize
          @data << name << "\0"
          index
        end
      end
    end

    def initialize
      @text = ""
      @const = nil
      @bss_size = 0
      @reloc_offsets = []
      @symtab = nil
    end

    def text(data)
      @text = data
      self
    end

    def const(data)
      @const = data
      self
    end

    def bss(size)
      @bss_size = size
      self
    end

    # Offsets in the text section that hold an address into .bss.
    def reloc(offsets)
      @reloc_offsets = offsets
      self
    end

    def symtab(symtab)
      @symtab = symtab
      self
    end

    def serialize
      # Sections come first in this order, and their section symbols
      # are laid out the same way, so a section's index doubles as the
      # index of its symbol.
      shndx = {text: 1}
      shndx[:const] = shndx.size + 1 if @const
      shndx[:bss] = shndx.size + 1

      strtab = StringTable.new
      symbols = symbol_table(shndx, strtab)
      first_global = symbols.index { |sym| sym[:st_info] >> 4 == STB_GLOBAL } || symbols.size
      rels = @reloc_offsets.map { |offset| Rel.new(offset, ELF.r_info(shndx[:bss], R_386_32)) }

      shstrtab = StringTable.new
      sections = [Section.new(name_index: 0, type: SHT_NULL, flags: 0, data: "", size: 0, link: 0,
        info: 0, align: 0, entsize: 0)]
      section = lambda do |name, type, flags, data, size: data.bytesize, link: 0, info: 0, align: 1, entsize: 0|
        sections << Section.new(name_index: shstrtab.add(name), type:, flags:, data:, size:,
          link:, info:, align:, entsize:)
        sections.size - 1
      end

      section.call(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, @text, align: 16)
      section.call(".data", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE, @const, align: 4) if @const
      section.call(".bss", SHT_NOBITS, SHF_ALLOC | SHF_WRITE, "", size: @bss_size, align: 4)
      symtab_index = sections.size
      section.call(".symtab", SHT_SYMTAB, 0, symbols.map(&:serialize).join,
        link: symtab_index + 1, info: first_global, align: 4, entsize: Sym.bytesize)
      section.call(".strtab", SHT_STRTAB, 0, strtab.data)
      section.call(".rel.text", SHT_REL, 0, rels.map(&:serialize).join, link: symtab_index,
        info: shndx[:text], align: 4, entsize: Rel.bytesize)
      shstrtab_index = section.call(".shstrtab", SHT_STRTAB, 0, "")
      sections[shstrtab_index] = sections[shstrtab_index].with(data: shstrtab.data,
        size: shstrtab.data.bytesize)

      # Section data follows the header, then the section header table.
      blobs = ""
      headers = []
      offset = Header.bytesize
      sections.each do |sect|
        padding = (sect.align > 1) ? (-offset) % sect.align : 0
        blobs << "\0" * padding
        offset += padding
        headers << SectionHeader.new(sect.name_index, sect.type, sect.flags, 0, offset, sect.size,
          sect.link, sect.info, sect.align, sect.entsize)
        blobs << sect.data
        offset += sect.data.bytesize
      end
      padding = (-offset) % 4
      blobs << "\0" * padding
      shoff = offset + padding

      ident = ELFMAG + [ELFCLASS32, ELFDATA2LSB, EV_CURRENT].pack("C*")
      header = Header.new(ident, ET_REL, EM_386, EV_CURRENT, 0, 0, shoff, 0, Header.bytesize, 0, 0,
        SectionHeader.bytesize, sections.size, shstrtab_index)

      header.serialize + blobs + headers.map(&:serialize).join
    end

    #######
    private

    #######

    # The null symbol, one symbol per section, then locals, then globals.
    def symbol_table(shndx, strtab)
      null = Sym.new(0, 0, 0, 0, 0, 0)
      section_symbols = shndx.map do |_, index|
        Sym.new(0, 0, 0, ELF.st_info(STB_LOCAL, STT_SECTION), 0, index)
      end
      symbols = (@symtab.local_symbols + @symtab.global_symbols).map do |sym|
        Sym.new(strtab.add(sym.name), sym.value, 0, ELF.st_info(sym.bind, sym.type), 0,
          shndx.fetch(sym.section))
      end
      [null] + section_symbols + symbols
    end
  end
end
