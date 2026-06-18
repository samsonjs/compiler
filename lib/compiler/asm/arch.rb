class Compiler
  module ASM

    class Arch

      attr_reader :bits, :word_bits
      attr_reader :preamble, :postamble
      attr_reader :endianness

      # config:
      #   - bits: native register / pointer size
      #   - word_bits: number of bits in a word
      #   - endianness: "big" or "little"
      #   - preamble: binary preamble
      #   - postamble: binary postamble
      def initialize(config)
        @bits = config['bits']
        @word_bits = config['word_bits']
        @endianness = config['endianness']
        @preamble = config['preamble']
        @postamble = config['postamble']
      end

      def bytes
        bits / 8
      end

      def word_bytes
        word_bits / 8
      end

      def big_endian?
        endianness == 'big'
      end

      def little_endian?
        endianness == 'little'
      end

      def pointer_bytes
        bytes
      end

      def min_signed
        -1 * 2 ** (bits - 1)
      end

      def max_signed
        2 ** (bits - 1) - 1
      end

      def min_unsigned
        0
      end

      def max_unsigned
        2 ** bits - 1
      end

      def signed_int
        @signed_int ||= min_signed..max_signed
      end

    end

  end
end
