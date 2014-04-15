#
#
#
#

require "./c6502"


module Assembler

  class C65816 < C6502
  	A = Register.new(:A)
  	S = Register.new(:S)

  	
    #################### Single-byte no-argument opcodes (extra from 6502)
    single_byte {
      :phd => 0x0b,
      :tcs => 0x1b,
      :pld => 0x2b,
      :tsc => 0x3b,
      :phk => 0x4b,
      :tcd => 0x5b,
      :rtl => 0x6b,
      :tdc => 0x7b,
      :phb => 0x8b,
      :txy => 0x9b,
      :plb => 0xab,
      :tyx => 0xbb,
      :wai => 0xcb,
      :stp => 0xdb,
      :xba => 0xeb,
      :xce => 0xfb
    }

    
  	def cop(x); db 1, x.if_within(0, 255).b end

    



  end
end