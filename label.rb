require "./number_addons"

module Assembler

  class UnresolvedLabel
    def initialize label
      @label = label
    end
    
    def throw_up; raise "unresolved label #{@label}" end
    
    def to_i; self.throw_up end
    def distance_to(p); self.throw_up end
    def lo_byte; self.throw_up end
    def hi_byte; self.throw_up end
    def lo_word; self.throw_up end
    def hi_word; self.throw_up end
    def bcd_digit(d); self.throw_up end
    def b; self.throw_up end
    def w; self.throw_up end
    def d; self.throw_up end
    def byte_index(i); self.throw_up end
    
    def is_within? (a, b) self.throw_up end
    def if_within (a, b) self.throw_up end

    def if_byte; self.throw_up end
    def if_word; self.throw_up end
    def if_dword; self.throw_up end
  end

  class LabelPromise
    def initialize label
      @label = label
      @op_queue = []
    end

    def commit
      value = @label.to_i
      @ops.each {|op| value = op(value)}
      value
    end
    def to_i; @ops.push -> v { v.to_i }; self end
    def -(); @ops.push -> v { -v.to_i }; self end
    def +(d); @ops.push -> v { v.to_i + d.to_i }; self end
    def relative_to(p); @ops.push -> v {v.relative_to p}; self end
    def lo_byte; @ops.push -> v {v.lo_byte}; self end
    def hi_byte; @ops.push -> v {v.hi_byte}; self end
    def lo_word; @ops.push -> v {v.lo_word}; self end
    def hi_word; @ops.push -> v {v.hi_word}; self end
    
    def bcd_digit(d); @ops.push -> v{v.bcd_digit d}; self end
    def b; @ops.push -> v {v.b}; self end
    def w; @ops.push -> v {v.w}; self end
    def d; @ops.push -> v {v.d}; self end
    def byte_index(i); @ops.push -> v {v.byte_index i}; self end
    
    def is_within? (a, b) self.commit.is_within? a, b end
    def if_within (a, b) @ops.push -> v {v.if_within a, b}; self end

    def if_byte; @ops.push -> v {v.if_byte}; self end
    def if_word; @ops.push -> v {v.if_word}; self end
    def if_dword; @ops.push -> v {v.if_dword}; self end 
  end

  class ::Symbol
    def to_i
      Assembler.current.label_ref self
    end
    def -(); -(self.to_i) end
    def +(d); (self.to_i+d.to_i) end
    def relative_to(p); self.to_i.relative_to p end
    def lo_byte; self.to_i.lo_byte end
    def hi_byte; self.to_i.hi_byte end
    def lo_word; self.to_i.lo_word end
    def hi_word; self.to_i.hi_word end
    def bcd_digit(d); self.bcd_digit d end
    def b; self.to_i.b end
    def w; self.to_i.w end
    def d; self.to_i.d end
    def byte_index(i); self.to_i.byte_index i end
    
    def is_within? (a, b) self.to_i.is_within? a,b end
    def if_within (a, b) self.to_i.if_within a,b end

    def if_byte; self.to_i.if_byte end
    def if_word; self.to_i.if_word end
    def if_dword; self.to_i.if_dword end
  end



end