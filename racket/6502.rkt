#lang racket

(require "plasm.rkt")

(define (65imm? n)  (between? -128 n 255))
(define (65addr? n) (between? 0 n 65535))
(define (65rel? n)  (between? -128 (->@ n) 255))

(define (65imm op n)
  (db op n))
(define (65addr op n)
  (db op (asm-b 0 n) (asm-b 1 n)))
(define (65rel op n)
  (db op (->@ n)))

(define 6502-base
  (match-lambda
    [`(adc.izx ,(? 65imm? n))    (65imm  #x61 n)]
    [`(adc.zp  ,(? 65imm? n))    (65imm  #x65 n)]
    [`(adc.imm ,(? 65imm? n))    (65imm  #x69 n)]
    [`(adc.abs ,(? 65addr? n))   (65addr #x6D n)]
    [`(adc.izy ,(? 65imm? n))    (65imm  #x71 n)]
    [`(adc.zpx ,(? 65imm? n))    (65imm  #x75 n)]
    [`(adc.aby ,(? 65addr? n))   (65addr #x79 n)]
    [`(adc.abx ,(? 65addr? n))   (65addr #x7D n)]
    
    [`(and.izx ,(? 65imm? n))    (65imm  #x21 n)]
    [`(and.zp  ,(? 65imm? n))    (65imm  #x25 n)]
    [`(and.imm ,(? 65imm? n))    (65imm  #x29 n)]
    [`(and.abs ,(? 65addr? n))   (65addr #x2D n)]
    [`(and.izy ,(? 65imm? n))    (65imm  #x31 n)]
    [`(and.zpx ,(? 65imm? n))    (65imm  #x35 n)]
    [`(and.aby ,(? 65addr? n))   (65addr #x39 n)]
    [`(and.abx ,(? 65addr? n))   (65addr #x3D n)]
    
    [`(asl.zp  ,(? 65imm? n))    (65imm #x06 n)]
    [`(asl)                      (db #x0a)]
    [`(asl.abs ,(? 65addr? n))   (65addr #x0e n)]
    [`(asl.zpx ,(? 65imm? n))    (65imm  #x16 n)]
    [`(asl.abx ,(? 65addr? n))   (65addr #x1e n)]
    
    [`(bcc ,(? 65rel? n))        (65rel #x90 n)]
    [`(bcs ,(? 65rel? n))        (65rel #xb0 n)]
    [`(beq ,(? 65rel? n))        (65rel #xf0 n)]
    [`(bmi ,(? 65rel? n))        (65rel #x30 n)]
    [`(bne ,(? 65rel? n))        (65rel #xd0 n)]
    [`(bpl ,(? 65rel? n))        (65rel #x10 n)]
    [`(bvc ,(? 65rel? n))        (65rel #x50 n)]
    [`(bvs ,(? 65rel? n))        (65rel #x70 n)]
    
    [`(bit.zp  ,(? 65imm? n))    (65imm #x24 n)]
    [`(bit.abs ,(? 65addr? n))   (65addr #x3C n)]
    
    [`(brk ,(? 65imm? n))        (65imm #x00 n)]
    [`(brk)                      (db 0)]

    [`(clc)                      (db #x18)]
    [`(cld)                      (db #xd8)]
    [`(cli)                      (db #x58)]
    [`(clv)                      (db #x68)]
    
    [`(cmp.izx ,(? 65imm? n))    (65imm  #xc1 n)]
    [`(cmp.zp  ,(? 65imm? n))    (65imm  #xc5 n)]
    [`(cmp.imm ,(? 65imm? n))    (65imm  #xc9 n)]
    [`(cmp.abs ,(? 65addr? n))   (65addr #xcd n)]
    [`(cmp.izy ,(? 65imm? n))    (65imm  #xd1 n)]
    [`(cmp.zpx ,(? 65imm? n))    (65imm  #xd2 n)]
    [`(cmp.aby ,(? 65addr? n))   (65addr #xd5 n)]
    [`(cmp.abx ,(? 65addr? n))   (65addr #xd9 n)]
    
    [`(cpx.imm ,(? 65imm? n))    (65imm  #xe0 n)]
    [`(cpx.zp  ,(? 65imm? n))    (65imm  #xe4 n)]
    [`(cpx.abs ,(? 65addr? n))   (65addr #xec n)]
    
    [`(cpy.imm ,(? 65imm? n))    (65imm  #xc0 n)]
    [`(cpy.zp  ,(? 65imm? n))    (65imm  #xc4 n)]
    [`(cpy.abs ,(? 65addr? n))   (65addr #xcc n)]
    
    [`(dec.zpx ,(? 65imm? n))    (65imm #xd6 n)]
    [`(dec.abx ,(? 65addr? n))   (65addr #xde n)]
    [`(dec.zp  ,(? 65imm? n))    (65imm #xc6 n)]
    [`(dec.abs ,(? 65addr? n))   (65addr #xce n)]
    
    [`(dex)                      (db #xca)]
    [`(dey)                      (db #x88)]
    
    [`(eor.izx ,(? 65imm? n))    (65imm  #x41 n)]
    [`(eor.zp  ,(? 65imm? n))    (65imm  #x45 n)]
    [`(eor.imm ,(? 65imm? n))    (65imm  #x49 n)]
    [`(eor.abs ,(? 65addr? n))   (65addr #x4d n)]
    [`(eor.izy ,(? 65imm? n))    (65imm  #x51 n)]
    [`(eor.zpx ,(? 65imm? n))    (65imm  #x55 n)]
    [`(eor.aby ,(? 65addr? n))   (65addr #x59 n)]
    [`(eor.abx ,(? 65imm? n))    (65imm  #x59 n)]
    
    [`(inc ,(? 65imm? n) x)      (65imm #xf6 n)]
    [`(inc ,(? 65addr? n) x)    (65addr #xfe n)]
    [`(inc ,(? 65imm? n))        (65imm #xe6 n)]
    [`(inc ,(? 65addr? n))      (65addr #xee n)]
    
    [`(inx)                      (db #xe8)]
    [`(iny)                      (db #xc8)]
    
    [`(jmp.ind ,(? 65addr? n))   (65addr #x6c n)]
    [`(jmp ,(? 65addr? n))       (65addr #x4c n)]
    
    [`(jsr ,(? 65addr? n))       (65addr #x20 n)]
    
    [`(lda.izx ,(? 65imm? n))    (65imm  #xA1 n)]
    [`(lda.zp  ,(? 65imm? n))    (65imm  #xA5 n)]
    [`(lda.imm ,(? 65imm? n))    (65imm  #xA9 n)]
    [`(lda.abs ,(? 65addr? n))   (65addr #xad n)]
    [`(lda.izy ,(? 65imm? n))    (65imm  #xB1 n)]
    [`(lda.zpx ,(? 65imm? n))    (65imm  #xb5 n)]
    [`(lda.aby ,(? 65addr? n))   (65addr #xb9 n)]
    [`(lda.abx ,(? 65addr? n))   (65addr #xbd n)]
    
    [`(ldx.imm ,(? 65imm? n))    (65imm  #xa2 n)]
    [`(ldx.zp  ,(? 65imm? n))    (65imm  #xa6 n)]
    [`(ldx.abs ,(? 65addr? n))   (65addr #xae n)]
    [`(ldx.zpy ,(? 65imm? n))    (65imm  #xb6 n)]
    [`(ldx.aby ,(? 65addr? n))   (65addr #xbe n)]
    
    [`(ldy.imm ,(? 65imm? n))    (65imm  #xa0 n)]
    [`(ldy.zp ,(? 65imm? n))     (65imm  #xa4 n)]
    [`(ldy.abs ,(? 65addr? n))   (65addr #xac n)]
    [`(ldy.zpx ,(? 65imm? n))    (65imm  #xb4 n)]
    [`(ldy.abx ,(? 65addr? n))   (65addr #xbc n)]
    
    [`(lsr.zp ,(? 65imm? n))     (65imm  #x46 n)]
    [`(lsr.a)                    (db     #x4a)]
    [`(lsr.abs ,(? 65addr? n))   (65addr #x4e n)]
    [`(lsr.zpx ,(? 65imm? n))    (65imm  #x56 n)]
    [`(lsr.abx ,(? 65addr? n))   (65addr #x5e n)]
    
    [`(nop)                      (db #xea)]
    
    [`(ora.izx ,(? 65imm? n))    (65imm  #x01 n)]
    [`(ora.zp  ,(? 65imm? n))    (65imm  #x05 n)]
    [`(ora.imm ,(? 65imm? n))    (65imm  #x09 n)]
    [`(ora.abs ,(? 65addr? n))   (65addr #x0d n)]
    [`(ora.izy ,(? 65imm? n))    (65imm  #x11 n)]
    [`(ora.zpx ,(? 65imm? n))    (65imm  #x15 n)]
    [`(ora.aby ,(? 65addr? n))   (65addr #x19 n)]
    [`(ora.abx ,(? 65addr? n))   (65addr #x1d n)]
    
    [`(pha)                      (db #x48)]
    [`(php)                      (db #x08)]
    [`(pla)                      (db #x68)]
    [`(plp)                      (db #x28)]
    
    [`(rol.zp ,(? 65imm? n))     (65imm  #x26 n)]
    [`(rol.a)                    (db     #x2a)]
    [`(rol.abs ,(? 65addr? n))   (65addr #x2e n)]
    [`(rol.zpx ,(? 65imm? n))    (65imm  #x36 n)]
    [`(rol.abx ,(? 65addr? n))   (65addr #x3e n)]
    
    [`(ror.zp ,(? 65imm? n))     (65imm  #x66 n)]
    [`(ror.a)                    (db     #x6a)]
    [`(ror.abs ,(? 65addr? n))   (65addr #x6e n)]
    [`(ror.zpx ,(? 65imm? n))    (65imm  #x76 n)]
    [`(ror.abx ,(? 65addr? n))   (65addr #x7e n)]
    
    [`(rti)                      (db #x40)]
    [`(rts)                      (db #x60)]
    
    [`(sbc.izx ,(? 65imm? n))    (65imm  #xe1 n)]
    [`(sbc.zp  ,(? 65imm? n))    (65imm  #xe5 n)]
    [`(sbc.imm ,(? 65imm? n))    (65imm  #xe9 n)]
    [`(sbc.abs ,(? 65addr? n))   (65addr #xed n)]
    [`(sbc.izy ,(? 65imm? n))    (65imm  #xf1 n)]
    [`(sbc.zpx ,(? 65imm? n))    (65imm  #xf5 n)]
    [`(sbc.aby ,(? 65addr? n))   (65addr #xf9 n)]
    [`(sbc.abx ,(? 65addr? n))   (65addr #xfd n)]
    
    [`(sec)                      (db #x38)]
    [`(sei)                      (db #x78)]
    [`(sed)                      (db #xf8)]
    
    [`(sta.izx ,(? 65imm? n))    (65imm  #x81 n)]
    [`(sta.zp  ,(? 65imm? n))    (65imm  #x85 n)]
    [`(sta.imm ,(? 65imm? n))    (65imm  #x89 n)]
    [`(sta.abs ,(? 65addr? n))   (65addr #x8d n)]
    [`(sta.izy ,(? 65imm? n))    (65imm  #x91 n)]
    [`(sta.zpx ,(? 65imm? n))    (65imm  #x95 n)]
    [`(sta.aby ,(? 65addr? n))   (65addr #x99 n)]
    [`(sta.abx ,(? 65addr? n))   (65addr #x9d n)]
    
    [`(stx.zp  ,(? 65imm? n))    (65imm  #x86 n)]
    [`(stx.abs ,(? 65addr? n))   (65addr #x8e n)]
    [`(stx.zpy ,(? 65imm? n))    (65imm  #x96 n)]
    
    [`(sty.zp  ,(? 65imm? n))    (65imm  #x84 n)]
    [`(sty.abs ,(? 65addr? n))   (65addr #x8c n)]
    [`(sty.zpx ,(? 65imm? n))    (65imm  #x94 n)]
    
    [`(tay)                      (db #xa8)]
    [`(tax)                      (db #xaa)]
    [`(tsx)                      (db #xba)]
    [`(txa)                      (db #x8a)]
    [`(txs)                      (db #x9a)]
    [`(tya)                      (db #x98)]
    
    [rest (%asm-base rest)]
    ))
(define 65c02
  (match-lambda
    
    [`(adc.zpi ,(? 65imm? n))    (65imm  #x72 n)]
    [`(and.zpi ,(? 65imm? n))    (65imm  #x32 n)]
    [`(cmp.zpi ,(? 65imm? n))    (65imm  #xd2 n)]
    [`(eor.zpi ,(? 65imm? n))    (65imm  #x52 n)]
    [`(lda.zpi ,(? 65imm? n))    (65imm  #xb2 n)]
    [`(ora.zpi ,(? 65imm? n))    (65imm  #x12 n)]
    [`(sbc.zpi ,(? 65imm? n))    (65imm  #xf2 n)]
    [`(sta.zpi ,(? 65imm? n))    (65imm  #x92 n)]
    
    [`(bra ,(? 65rel? n))        (65rel #x80 n)]
    
    [`(jmp.abx ,(? 65addr? a)) (65addr #x7c a)]
    
    [`(dea)                      (db #x3a)]
    [`(ina)                      (db #x1a)]
    
    [`(phx) (db #xda)]
    [`(phy) (db #x5a)]
    [`(plx) (db #xfa)]
    [`(ply) (db #x7a)]
    
    [`(stz.zp ,(? 65imm? zp))  (db #x64 zp)]
    [`(stz.zpx ,(? 65imm? zp)) (db #x74 zp)]
    [`(stz.abs ,(? 65addr? addr)) (65addr #x9c addr)]
    [`(stz.abx ,(? 65addr? addr)) (65addr #x9e addr)]

    [`(trb.zp ,(? 65imm? zp)) (db #x14 zp)]
    [`(trb.abs ,(? 65addr? a)) (65addr #x1c a)]
    
    [`(tsb.zp ,(? 65imm? zp)) (db #x04 zp)]
    [`(tsb.abs ,(? 65addr? a)) (65addr #x0c a)]

    [rest (6502-base rest)]))

(define r65c02
  (match-lambda
    [`(bbr0 ,(? 65imm? zp) ,(? 65rel? disp)) (db #x0f zp (->@ disp))] 
    [`(bbr1 ,(? 65imm? zp) ,(? 65rel? disp)) (db #x1f zp (->@ disp))] 
    [`(bbr2 ,(? 65imm? zp) ,(? 65rel? disp)) (db #x2f zp (->@ disp))] 
    [`(bbr3 ,(? 65imm? zp) ,(? 65rel? disp)) (db #x3f zp (->@ disp))] 
    [`(bbr4 ,(? 65imm? zp) ,(? 65rel? disp)) (db #x4f zp (->@ disp))] 
    [`(bbr5 ,(? 65imm? zp) ,(? 65rel? disp)) (db #x5f zp (->@ disp))] 
    [`(bbr6 ,(? 65imm? zp) ,(? 65rel? disp)) (db #x6f zp (->@ disp))] 
    [`(bbr7 ,(? 65imm? zp) ,(? 65rel? disp)) (db #x7f zp (->@ disp))] 
    
    [`(bbs0 ,(? 65imm? zp) ,(? 65rel? disp)) (db #x8f zp (->@ disp))] 
    [`(bbs1 ,(? 65imm? zp) ,(? 65rel? disp)) (db #x9f zp (->@ disp))] 
    [`(bbs2 ,(? 65imm? zp) ,(? 65rel? disp)) (db #xaf zp (->@ disp))] 
    [`(bbs3 ,(? 65imm? zp) ,(? 65rel? disp)) (db #xbf zp (->@ disp))] 
    [`(bbs4 ,(? 65imm? zp) ,(? 65rel? disp)) (db #xcf zp (->@ disp))] 
    [`(bbs5 ,(? 65imm? zp) ,(? 65rel? disp)) (db #xdf zp (->@ disp))] 
    [`(bbs6 ,(? 65imm? zp) ,(? 65rel? disp)) (db #xef zp (->@ disp))] 
    [`(bbs7 ,(? 65imm? zp) ,(? 65rel? disp)) (db #xff zp (->@ disp))] 
    
    [`(rmb0 ,(? 65imm? zp)) (db #x07 zp)]
    [`(rmb1 ,(? 65imm? zp)) (db #x17 zp)]
    [`(rmb2 ,(? 65imm? zp)) (db #x27 zp)]
    [`(rmb3 ,(? 65imm? zp)) (db #x37 zp)]
    [`(rmb4 ,(? 65imm? zp)) (db #x47 zp)]
    [`(rmb5 ,(? 65imm? zp)) (db #x57 zp)]
    [`(rmb6 ,(? 65imm? zp)) (db #x67 zp)]
    [`(rmb7 ,(? 65imm? zp)) (db #x77 zp)]
    
    [`(smb0 ,(? 65imm? zp)) (db #x87 zp)]
    [`(smb1 ,(? 65imm? zp)) (db #x97 zp)]
    [`(smb2 ,(? 65imm? zp)) (db #xa7 zp)]
    [`(smb3 ,(? 65imm? zp)) (db #xb7 zp)]
    [`(smb4 ,(? 65imm? zp)) (db #xc7 zp)]
    [`(smb5 ,(? 65imm? zp)) (db #xd7 zp)]
    [`(smb6 ,(? 65imm? zp)) (db #xe7 zp)]
    [`(smb7 ,(? 65imm? zp)) (db #xf7 zp)]

    [rest (65c02 rest)]))

(define huc6280
  (match-lambda
    
    [`(csh) (db #xd4)]
    [`(csl) (db #x54)]
    
    [`(st0 ,(? 65imm? imm)) (db #x03 imm)]
    [`(st1 ,(? 65imm? imm)) (db #x13 imm)]
    [`(st2 ,(? 65imm? imm)) (db #x23 imm)]
    
    [`(clx) (db #x82)]
    [`(cly) (db #xc2)]
    [`(cla) (db #x62)]
    
    [`(set) (db #xf4)]
    
    [`(tai ,(? 65addr? src) ,(? 65addr? dst) ,(? 65addr? loop))
     (db #xf3 (asm-b 0 src) (asm-b 1 src) (asm-b 0 dst) (asm-b 1 dst) (asm-b 0 loop) (asm-b 1 loop))]
    [`(tia ,(? 65addr? src) ,(? 65addr? dst) ,(? 65addr? loop))
     (db #xe3 (asm-b 0 src) (asm-b 1 src) (asm-b 0 dst) (asm-b 1 dst) (asm-b 0 loop) (asm-b 1 loop))]
    [`(tdd ,(? 65addr? src) ,(? 65addr? dst) ,(? 65addr? loop))
     (db #xc3 (asm-b 0 src) (asm-b 1 src) (asm-b 0 dst) (asm-b 1 dst) (asm-b 0 loop) (asm-b 1 loop))]
    [`(tin ,(? 65addr? src) ,(? 65addr? dst) ,(? 65addr? loop))
     (db #xd3 (asm-b 0 src) (asm-b 1 src) (asm-b 0 dst) (asm-b 1 dst) (asm-b 0 loop) (asm-b 1 loop))]
    [`(tii ,(? 65addr? src) ,(? 65addr? dst) ,(? 65addr? loop))
     (db #x73 (asm-b 0 src) (asm-b 1 src) (asm-b 0 dst) (asm-b 1 dst) (asm-b 0 loop) (asm-b 1 loop))]
    
    
    [`(sxy) (db #x02)]
    
    [`(tam ,(? 65imm? n)) (db #x53 n)]
    [`(tam0) (db #x53 1)]
    [`(tam1) (db #x53 2)]
    [`(tam2) (db #x53 4)]
    [`(tam3) (db #x53 8)]
    [`(tam4) (db #x53 16)]
    [`(tam5) (db #x53 32)]
    [`(tam6) (db #x53 64)]
    [`(tam7) (db #x53 128)]
    
    [`(tma ,(? 65imm? n)) (db #x43 n)]
    [`(tma0) (db #x43 1)]
    [`(tma1) (db #x43 2)]
    [`(tma2) (db #x43 4)]
    [`(tma3) (db #x43 8)]
    [`(tma4) (db #x43 16)]
    [`(tma5) (db #x43 32)]
    [`(tma6) (db #x43 64)]
    [`(tma7) (db #x43 128)]
    
    
    [`(tst.zp  ,(? 65imm? m) ,(? 65imm? zp)) (db #x83 m zp)]
    [`(tst.zpx ,(? 65imm? m) ,(? 65imm? zp)) (db #xa3 m zp)]
    [`(tst.abs ,(? 65imm? m) ,(? 65addr? a)) (db #x93 m (asm-b 0 a) (asm-b 1 a))]
    [`(tst.abx ,(? 65imm? m) ,(? 65addr? a)) (db #xb3 m (asm-b 0 a) (asm-b 1 a))]
    
    
    
    [rest (r65c02 rest)]))

(make-architecture '6502 #f 6502-base)
(make-architecture '65c02 #f 65c02)
(make-architecture 'huc6280 #f huc6280)