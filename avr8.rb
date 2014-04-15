#
#
#
#
#

require "assembler"

module Assembler
  
  class Avr8 < Assembler
    R0 = Register(:R0)
    R1 = Register(:R1)
    R2 = Register(:R2)
    R3 = Register(:R3)
    R4 = Register(:R4)
    R5 = Register(:R5)
    R6 = Register(:R6)
    R7 = Register(:R7)
    R8 = Register(:R8)
    R9 = Register(:R9)
  
    R10 = Register(:R10)
    R11 = Register(:R11)
    R12 = Register(:R12)
    R13 = Register(:R13)
    R14 = Register(:R14)
    R15 = Register(:R15)
    R16 = Register(:R16)
    R17 = Register(:R17)
    R18 = Register(:R18)
    R19 = Register(:R19)
  
    R20 = Register(:R20)
    R21 = Register(:R21)
    R22 = Register(:R22)
    R23 = Register(:R23)
    R24 = Register(:R24)
    R25 = Register(:R25)
    R26 = Register(:R26)
    R27 = Register(:R27)
    R28 = Register(:R28)
    R29 = Register(:R29)

    R30 = Register(:R30)
    R31 = Register(:R31)
    
    X = Register(:X)
    Y = IndexableRegister(:Y)
    Z = IndexableRegister(:Z)
    
    REGISTERS = {
      R0=>0, R1=>1, R2=>2, R3=>3, R4=>4,
      R5=>5, R6=>6, R7=>7, R8=>8, R9=>9,
      R10=>10, R11=>11, R12=>12, R13=>13, R14=>14,
      R15=>15, R16=>16, R17=>17, R18=>18, R19=>19,
      R20=>20, R21=>21, R22=>22, R23=>23, R24=>24,
      R25=>25, R26=>26, R27=>27, R28=>28, R29=>29,
      R30=>30, R31=>31}
    LDI_REGISTERS = [R16, R17, R18, R19,
                    R20, R21, R22, R23, R24, R25, R26, R27, R28, R29,
                    R30, R31]
    MOVW_EVEN_REGISTERS = [R0, R2, R4, R6, R8,
                    R10, R12, R14, R16, R18,
                    R20, R22, R24, R26, R28,
                    R30]
    FMUL_REGISTERS = {R16=>0, R17=>1, R18=>2, R19=>3,
                     R20=>4, R21=>5, R22=>6, R23=>7}
    ADIW_REGISTERS = {R24=>0, R26=>1, R28=>2, R30=>3}
    POINTER_REGISTERS = {X=>0, Y=>1, Z=>2}
    
    def initialize (device)
      super
      @device = device
      @big_endian = true
    end
    
    ############## no-parameter opcodes
    def nop; dw 0 end
    def ret; dw 0x9504 end
    def reti; dw 0x9514 end
    def icall; dw 0x9509 end
    def ijmp; dw 0x9409 end
    def eicall; dw 0x9519 end
    def eijmp; dw 0x9419 end
    def sleep; dw 0x9588 end
    def break_; dw 0x9598 end
    def wdr; dw 0x95a8 end
    def spm; dw 0x95e8 end
    
    def sec; dw 0x9408 end
    def sez; dw 0x9418 end
    def sen; dw 0x9428 end
    def sev; dw 0x9438 end
    def ses; dw 0x9448 end
    def seh; dw 0x9458 end
    def set; dw 0x9468 end
    def sei; dw 0x9478 end
    
    def clc; dw 0x9488 end
    def clz; dw 0x9498 end
    def cln; dw 0x94a8 end
    def clv; dw 0x94b8 end
    def cls; dw 0x94c8 end
    def clh; dw 0x94d8 end
    def clt; dw 0x94e8 end
    def cli; dw 0x94f8 end
    
    ###################### parametric opcodes
    
    def _bitpos(s)
      if (s.is_a? Fixnum)&&((0..7)===s)
        s
      else
        raise "invalid bit position parameter"
      end
    end
    
    def bclr(s); dw 0x9488+(_bitpos(s)<<4) end
    def bset(s); dw 0x9408+(_bitpos(s)<<4) end
    
    def lpm(r=nil, z=nil)
      if(r==nil)&&(z==nil)
        dw 0x95c8
      elsif (LDI_REGISTERS.contains? r)&&(Z===z)
        dw 0x9004+(REGISTERS[r]<<4)+(z.offset==1?1:0)
      end
    end
    
    def elpm(r=nil, z=nil)
      if(r==nil)&&(z==nil)
        dw 0x95d8
      elsif (LDI_REGISTERS.contains? r)&&(Z===z)
        dw 0x9006+(REGISTERS[r]<<4)+(z.offset==1?1:0)
      end
    end
    
    def _alu_op(offset, ra, rb)
      if(REGISTERS.key? ra)&&(REGISTERS.key? rb)
        dw offset+((REGISTERS[ra]&0x10)<<5)+(REGISTERS[rb]<<4)+(REGISTERS[ra]&0xf)
      else
        raise "operands should be registers (R0-R31)"
      end
    end
    
    def adc(ra,rb); _alu_op(0x1c00,ra,rb) end
    def add(ra,rb); _alu_op(0x0c00,ra,rb) end
    def sbc(ra,rb); _alu_op(0x0800,ra,rb) end
    def sub(ra,rb); _alu_op(0x1800,ra,rb) end
    def and(ra,rb); _alu_op(0x2400,ra,rb) end
    def cp (ra,rb); _alu_op(0x1400,ra,rb) end
    def cpse(ra,rb);_alu_op(0x1000,ra,rb) end
    def cpc(ra,rb); _alu_op(0x0400,ra,rb) end
    def eor(ra,rb); _alu_op(0x2400,ra,rb) end
    def mov(ra,rb); _alu_op(0x2c00,ra,rb) end
    def mul(ra,rb); _alu_op(0x9c00,ra,rb) end
    
  end
  
end