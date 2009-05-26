# Assembler container module.  Sub modules are Text and Binary, which
# both export the same interface for generating either assembly or
# machine code for x86.
# 
# sjs
# may 2009

module Assembler

  # Abstract class for common functionality between different code
  # generators.  Also defines somewhat of an interface that must be
  # implemented to be useful.
  class AssemblerBase

    attr_reader :platform

    def initialize(platform='linux', *args)
      @platform = platform
      @vars = {}                   # Symbol table, maps names to locations in BSS.
      @num_labels = 0              # Used to generate unique labels.
      @num_labels_with_suffix = Hash.new(0)

      # Maps names to locations.
      @labels = Hash.new {|h, key| raise "undefined label: #{key}"}
      
    end

    def block(*args, &blk)
      instance_eval(&blk)
    end

    def output
      raise "#{self.class} is supposed to implement this method!"
    end

    def var(name)
      @vars[name]
    end
    alias_method :var?, :var
    
    # Generate a unique label.
    def label(suffix=nil)
      @num_labels += 1
      if suffix
        @num_labels_with_suffix[suffix] += 1
        suffix = "_#{suffix}_#{@num_labels_with_suffix[suffix]}"
      end
      name = "L#{sprintf "%06d", @num_labels}#{suffix}"
      return name
    end

  end

end
