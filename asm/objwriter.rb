module Assembler
  
  class UnimplementedMethodError < RuntimeError; end
  
  
  # Abstract base class.
  class ObjWriter
  
    def write!(filename)
      File.open(filename, 'wb') do |file|
        file.print(serialize)
      end
    end

    def fail(name)
      raise UnimplementedMethodError, name
    end
    
    # These methods must be defined for most uses of the library.
    %w[header segment section text data bss symtab serialize].each do |name|
      define_method(name) { fail(name) }
    end
    
  end
  
end