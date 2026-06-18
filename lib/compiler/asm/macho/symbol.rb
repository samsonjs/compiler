require 'compiler/macho'

class Compiler
  module MachO

    class Symbol

      attr_accessor :name, :type, :segnum, :desc, :value

      def initialize(name, type, segnum, desc, value)
        @name = name
        @type = type
        @segnum = segnum
        @desc = desc
        @value = value
      end


      def to_nlist(strx)
        Nlist.new(strx, @type, @segnum, @desc, @value)
      end


      def to_s
        @name
      end

    end

  end
end
