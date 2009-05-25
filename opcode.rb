class OpCode
  Attrs = [:prefix, :op, :modrm, :sib, :extra]
  attr_accessor *Attrs

  def initialize(attrs)
    Attrs.each do |attr|
      send("#{attr}=", attrs[attr])
    end
  end

  def size
    Attrs.inject(0) {|sum, attr|
      iv = instance_variable_get("@#{attr}")
      if iv.is_a?(Enumerable)
        sum + iv.size
      else
        sum + 1
      end
    }
  end

  def binary
    Attrs.map {|attr| send(attr)}.flatten.pack('c*')
  end
end
