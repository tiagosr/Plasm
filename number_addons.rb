#
# Ruby Macro Assembler
# (c)2011 Tiago Rezende
#
# Number-type addons for dealing with addresses and immediate values
#

module Assembler

  class ::Fixnum
    def relative_to(d); (- d.to_i) + self end
    def lo_byte; self & 0xff end
    def hi_byte; (self >> 8) & 0xff end
    def lo_word; self & 0xffff end
    def hi_word; (self >> 16) & 0xffff end
    def bcd_digit d
      (self / (10**d))%10
    end
    def b; self & 0xff end
    def w; self & 0xffff end
    def d; self & 0xffffffff end

    def byte_index i
      (self >> (8*i)) & 0xff
    end
    
    def is_within? (a, b)
      return (self >= a)&&(self <= b)
    end
    def if_within (a, b)
      if is_within?(a, b)
        return self
      else
        raise "out of range"
      end
    end

    def if_byte
      self.if_within(-128, 255).b
    end
    def if_word
      self.if_within(-32768, 65535).w
    end
    def if_dword
      self.if_within(-0x80000000, 0xffffffff).d
    end
  end


end