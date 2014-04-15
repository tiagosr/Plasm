

require './sms'

vavava = Assembler::SMS.new
vavava.asm do
  org 0
  jr :Start

  org 38
  section "VBlank/HBlank interrupt service routine", {:force => true} do	
    ex AF, AF
    exx
    jp [HL]
  end

  org 0x66
  section "Pause button handling", {:force => true} do
  	push AF
  	ld A, [:PauseFlag]
  	xor 1
  	ld [:PauseFlag], A
  	pop AF
  	retn
  end


  __ :Start
  di # disable ints
  im 1
  ld SP, 0xdff0
  call :InitVDP
  xor A
  ld [:PauseFlag], A # set pauseflag to zero
  ei # re-enable ints

  rst 0x38
  neg
end

vavava.link.write("vavava.sms")