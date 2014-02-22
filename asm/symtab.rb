module Assembler


  # Abstract symbol table.
  #
  # Basically a big map of variable, constant, and label names to
  # offsets within their respective sections.  Final addresses are
  # calculated from these offsets on the 2nd pass when we know where
  # things will actually live in memory.

  class Symtab

    attr_accessor :text_offset, :bss_offset, :const_offset
    attr_reader :const_data, :const_size, :bss_size, :reloc_info

    def initialize
      @vars = {}                   # Map of variable names to offsets. (bss vars)
      @consts = {}                 # Map of constant names to offsets.
      @funcs = {}                  # map of function names to offsets.

      # Initial data to load into memory (data for __DATA segment).
      @const_data = ''

      @const_size = 0              # Size of const section.
      @bss_size = 0                # Size of bss section.

      # Map names to locations.
      @labels = Hash.new {|h, key| raise "undefined label: #{key}"}
      @num_labels = 0              # Used to generate unique labels.
      @num_labels_with_suffix = Hash.new(0)

      # Relocation info. Subclasses should define a reloc method.
      @reloc_info = []

      @text_offset = 0
      @bss_offset = 0
      @const_offset = 0
    end

    # Generate a unique label.
    def unique_label(suffix=nil)
      @num_labels += 1
      if suffix
        @num_labels_with_suffix[suffix] += 1
        suffix = "_#{suffix}_#{@num_labels_with_suffix[suffix]}"
      end
      name = "L#{sprintf "%06d", @num_labels}#{suffix}"
      return name
    end

    def deflabel(name, offset)
      @labels[name] = offset
      return name
    end


    def lookup_label(name)
      @labels[name]
    end


    def defvar(name, bytes)
      @vars[name] = @bss_size
      @bss_size += bytes
    end


    def defconst(name, value, bytes)
      @consts[name] = @const_size
      @const_size += bytes
      @const_data << [value].pack('i')
    end


    def defun(name, offset)
      @funcs[name] = offset
    end


    def var(name)
      @vars[name]
    end

    def var?(name)
      @vars.has_key?(name)
    end

    def const(name)
      @consts[name]
    end

    def const?(name)
      @consts.has_key?(name)
    end

  end

end
