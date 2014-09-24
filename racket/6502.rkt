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

(make-architecture
 '6502 #f
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
    [`(bra ,(? 65rel? n))        (65rel #x80 n)]
    
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
    
    [`(dea)                      (db #x3a)]
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
    
    [`(ina)                      (db #x1a)]
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
