class Compiler

  class ParseError < RuntimeError

    attr_reader :caller, :context

    def initialize(caller, context = nil)
      @caller = caller
      @context = context
    end

  end

end
