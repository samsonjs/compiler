require 'asm/regproxy'

module Assembler
  
  module Registers
    
    # This structure allows for x86 registers of all sizes.  The
    # number of the register is the index of the array in which it was
    # found.  The size of a register in bytes is 2 ** index-into-sub-array.
    Registers = [ [:al, :ax, :eax], # 0
                  [:cl, :cx, :ecx], # 1
                  [:dl, :dx, :edx], # 2
                  [:bl, :bx, :ebx], # 3
                  [:ah, :sp, :esp], # 4
                  [:ch, :bp, :ebp], # 5
                  [:dh, :si, :esi], # 6
                  [:bh, :di, :edi]  # 7
                ]

    # Setup register proxies which are used both in effective address
    # calculations, and also just as symbols representing registers.
    Registers.each_with_index do |group, regnum|
      group.each_with_index do |reg, i|
        name = reg.to_s.upcase
        const_set(name, RegisterProxy.new(reg, 8 * (2 ** i), regnum))
      end
    end
    
    
  end
  
end