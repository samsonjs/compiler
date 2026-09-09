module Compiler
  class ParseError < StandardError
    attr_reader :caller, :context
    def initialize(caller, context = nil)
      @caller = caller
      @context = context
    end
  end
end
