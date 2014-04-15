#
# Ruby Macro Assembler
# (c)2011 Tiago Rezende
#
# Motorola MC680x0 assembler
#

require "./assembler"


class M68000 < Assembler
  class M68K_AddrRegister < IndexableRegister
    def initialize (name, offset = 0, post=nil, pre=nil)
      super.initialize name, offset
      @pre = pre
      @post = post
    end
    def preinc
      M68K_AddrRegister.new @name, @offset, 1
    end
    def postinc
      M68K_AddrRegister.new @name, @offset, nil, 1
    end
    def predec
      M68K_AddrRegister.new @name, @offset, -1
    end
    def postdec
      M68K_AddrRegister.new @name, @offset, nil, -1
    end
    def pre; @pre end
    def post; @post end
  end
  A0 = M68K_AddrRegister.new(:A0)
  A1 = M68K_AddrRegister.new(:A1)
  A2 = M68K_AddrRegister.new(:A2)
  A3 = M68K_AddrRegister.new(:A3)
  A4 = M68K_AddrRegister.new(:A4)
  A5 = M68K_AddrRegister.new(:A5)
  A6 = M68K_AddrRegister.new(:A6)
  SP = A7 = M68K_AddrRegister.new(:A7)
  D0 = Register.new(:D0)
  D1 = Register.new(:D1)
  D2 = Register.new(:D2)
  D3 = Register.new(:D3)
  D4 = Register.new(:D4)
  D5 = Register.new(:D5)
  D6 = Register.new(:D6)
  D7 = Register.new(:D7)
  
  ADDRESS_REGS = [A0, A1, A2, A3, A4, A5, A6, A7]
  DATA_REGS = [D0, D1, D2, D3, D4, D5, D6, D7]

  ADDR_REGS_INDICES = {A0=>0, A1=>1, A2=>2, A3=>3, A4=>4, A5=>5, A6=>6, A7=>7}
  DATA_REGS_INDICES = {D0=>0, D1=>1, D2=>2, D3=>3, D4=>4, D5=>5, D6=>6, D7=>7}
  
  def initialize
    @big_endian = true
  end
  
  def abcd (dest, src)
    if ADDRESS_REGS.include? dest
      if ADDRESS_REGS.include? src
        if ((dest.pre == -1)&&(src.pre==-1)&&(dest.post==0)&&(src.post==0))
          dw (0xc108 + (ADDR_REGS_INDICES[src]<<9) + (ADDR_REGS_INDICES[dest]))
        else
          raise "opcode can only use address registers in predecrement mode"
        end
      else
        raise "opcode can only set an indirect address register destination for an indirect address register source"
      end
    elsif DATA_REGS.include? dest
      if DATA_REGS.include? src
        dw (0xc108 + (ADDR_REGS_INDICES[src]<<9) + (ADDR_REGS_INDICES[dest]))
      end
    else
      raise "invalid parameters"
    end  
  end
  
  class OpcodeModeGenerator
    def initialize (parent, op)
      @p = parent
      @op = op
    end
    def b(dest,src) 
      a, b = @op.call(dest,src)
      @p.dw(a)
      if (b.is_a? Fixnum)
        db 0, b
      end 
    end
    def w(dest,src)
      a, b = @op.call(dest,src)
      @p.dw(a+0x40)
      if (b.is_a? Fixnum)
        dw b
      end
    end
    def d(dest,src)
      op, b = @op.call(dest,src)
      @p.dw(op+0x80)
      if (b.is_a? Fixnum)
        dd b
      end
    end
  end
  class MoveOpcodeModeGenerator
    def initialize (parent, op)
      @p = parent
      @op = op
    end    
    def b(dest,src) 
      @op.call(0,dest,src)
    end
    def w(dest,src)
      @op.call(0x1000,dest,src)
    end
    def d(dest,src)
      @op.call(0x2000,dest,src)
    end
  end
  def move(dest,src)
    def move_op(op_off, dest, src)
      if dest.is_a? Register
        if src.is_a? Register
        else
        end
      else
      end
    end
    MoveOpcodeModeGenerator.new self, move_op
  end
	
end