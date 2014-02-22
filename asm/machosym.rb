require 'asm/macho'

module Assembler

  class MachOSym

    attr_accessor :name, :type, :segnum, :desc, :value

    def initialize(name, type, segnum, desc, value)
      @name = name
      @type = type
      @segnum = segnum
      @desc = desc
      @value = value
    end


    def to_nlist(strx)
      MachO::Nlist.new(strx, @type, @segnum, @desc, @value)
    end


    def to_s
      @name
    end

  end

end