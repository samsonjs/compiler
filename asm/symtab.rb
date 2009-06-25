module Assembler
  
  class Symtab
    
    attr_reader :const_data, :bss_size
    
    def initialize
      @vars = {}                   # Map of variable names to addresses. (bss vars)
      @consts = {}                 # Map of constant names to addresses.
      @funcs = {}                  # map of function names to addresses.
      
      # Initial data to load into memory (data for __DATA segment).
      @const_data = ''

      @const_size = 0              # Size of const section.
      @bss_size = 0                # Size of bss section.

      # Map names to locations.
      @labels = Hash.new {|h, key| raise "undefined label: #{key}"}
      @num_labels = 0              # Used to generate unique labels.
      @num_labels_with_suffix = Hash.new(0)      
    end
    
    
    ####
    ## NB: Concrete subclasses must define methods named:
    ##       bss_offset, and const_offset
    ####

    
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

    def deflabel(name, addr)
      @labels[name] = addr
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


    def defun(name, addr)
      @funcs[name] = addr
    end

    
    def var(name)
      bss_offset + @vars[name]
    end
    
    def var?(name)
      @vars[name]
    end
    
    def const(name)
      const_offset + @consts[name]
    end

    def const?(name)
      @consts[name]
    end
     
  end
  
end
