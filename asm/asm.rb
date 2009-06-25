# Assembler container module.  Sub modules are Text and Binary, which
# both export the same interface for generating either assembly or
# machine code for x86.
# 
# sjs
# may 2009

require 'asm/registers'

module Assembler

  # Abstract class for common functionality between different code
  # generators.  Also defines somewhat of an interface that must be
  # implemented to be useful.
  class AssemblerBase

    attr_reader :platform

    def initialize(platform)
      @platform = platform
    end

    def block(*args, &blk)
      instance_eval(&blk)
    end

  end

end
