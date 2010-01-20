module Assembler
  
  # Wrap a variable's address so that we can perform arithmetic on it
  # before resolving it when we know where things will go in memory.
  # All we do is catch arithmetic ops and then provide a means to
  # resolve a final addres by replaying them later.
  #
  # e.g. [symtab.var('i')] or [symtab.var('i') * 2]
  class VariableProxy

    attr_reader :name
    attr_accessor :ops
    
    def initialize(name, const=false)
      @name = name
      @const = const
      @ops = []
    end

    %w[+ * / - % & |].each do |op|
      define_method(op) do |*args|
        new_proxy = self.class.new(@name, @const)
        new_proxy.ops << [op, *args]
        return new_proxy
      end
    end

    # XXX should this perhaps use the offset instead?
    def resolve(base_addr)
      @ops.inject(base_addr) do |addr, op|
        addr.send(*op)
      end
    end
  
    def const?
      @const
    end

  end

end
