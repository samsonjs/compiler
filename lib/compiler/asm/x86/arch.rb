require 'compiler/asm/arch'

class Compiler
  module ASM
    module X86

      module Arch

        BINARY_PREAMBLE = {
          'linux' => [],

          'darwin' => [ 0x55,                  # push ebp
                        0x89, 0xe5,            # mov ebp, esp
                        0x81, 0xec, 8, 0, 0, 0 # sub esp, 8
                      ]
        }

        BINARY_POSTAMBLE = {
          'linux' => [ 0x89, 0xc3,         # mov ebx, eax (exit code)
                       0xb8, 1, 0, 0, 0,   # mov eax, 1
                       0xcd, 0x80          # int 0x80
                     ],

          'darwin' => [ 0xc9,       # leave
                        0xc3        # ret
                      ]
        }

        def self.instance
          @instance ||= ASM::Arch.new({
            'bits' => 32,
            'word_bits' => 16,
            'preamble' => BINARY_PREAMBLE,
            'postamble' => BINARY_POSTAMBLE
          })
        end

      end

    end
  end
end
