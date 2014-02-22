module Assembler

  # Acts like a register and can be used as the base or index in an
  # effective address.
  #
  # e.g. [EAX] or [ESI+EBX] or [EAX + 0xff] or [EAX + EDX * 2]
  class RegisterProxy

    attr_reader :name, :size, :regnum
    attr_reader :base, :index, :scale


    def initialize(name, size, regnum)
      @name = name # attrs are read-only so sharing is ok
      @size = size
      @regnum = regnum
      @base = self
    end


    def +(index)
      raise "index already specified" if @index
      new_reg = self.clone
      new_reg.instance_variable_set('@index', index)
      new_reg
    end


    def *(scale)
      raise "index must come first" unless @index
      raise "scale already specified" if scale
      raise "unsupported scale: #{scale}" unless scale.to_s.match(/^[1248]$/)
      @scale = scale
      self
    end


    def scale?
      @scale
    end


    def index?
      @index
    end


    def register?
      @scale.nil? && @index.nil?
    end



    def to_s
      @name.to_s +
        (@index ? "+#{@index}" : '') +
        (@scale ? "*#{@scale}" : '')
    end


    def inspect
      to_s
    end

  end

end