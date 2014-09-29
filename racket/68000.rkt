#lang racket

(require "plasm.rkt")

(define (68kimm.b? n)   (between? -128 n 255))
(define (68kimm.w? n)   (between? -32768 n 65535))
(define (68kimm.l? n)   (between? (- #x80000000) n #xffffffff))
(define (68kdisp.s? n)  (between? -128 n 127))
(define (68kdisp.w? n)  (between? -32768 n 32767))
(define (68krel.s? n)   (and (!= 2 n) (between? -128 (->@ (+ 2 n)) 127)))
(define (68krel.w? n)   (between? -32768 (->@ (+ 4 n)) 32767))
(define (68krel.l? n)   (between? (- #x80000000) (->@ (+ 6 n)) #x7fffffff))
(define (68kaddr.w? n)  (between? 0 n 65535))
(define (68kaddr.l? n)  (between? 0 n #xffffffff))
(define (68kbitindex? n) (between? 0 n 31))

(define (68krel.s n) (& 255 (->@ (+ 2 n))))
(define (68krel.w n) (->@ (+ 4 n)))
(define (68krel.l n) (let ([l (->@ (+ 6 n))])
                        (values (asm-w 1 l) (asm-w 0 l))))

(define (68kreg.s? reg)
  (member reg '(d0.w d1.w d2.w d3.w d4.w d5.w d6.w d7.w
                d0.l d1.l d2.l d3.l d4.l d5.l d6.l d7.l
                a0.w a1.w a2.w a3.w a4.w a5.w a6.w a7.w sp.w
                a0.l a1.l a2.l a3.l a4.l a5.l a6.l a7.l sp.l)))
(define (68kdreg? reg)
  (member reg '(d0 d1 d2 d3 d4 d5 d6 d7)))
(define (68kareg? reg)
  (member reg '(a0 a1 a2 a3 a4 a5 a6 a7 sp)))

(define (68kareg.posinc? reg)
  (member reg '((a0+)
                (a1+)
                (a2+)
                (a3+)
                (a4+)
                (a5+)
                (a6+)
                (a7+)
                (sp+))))
(define (68kareg.predec? reg)
  (member reg '((-a0)
                (-a1)
                (-a2)
                (-a3)
                (-a4)
                (-a5)
                (-a6)
                (-a7)
                (-sp))))
(define (68kreglist? l)
  (for/fold 
      ([in-list #t])
      ([item l])
    (and in-list
      (or (68kdreg? item)
          (68kareg? item)))))
(define 68kreglist-maskitem-posinc
  (match-lambda
    ['d0    #b0000000000000001]
    ['d1    #b0000000000000010]
    ['d2    #b0000000000000100]
    ['d3    #b0000000000001000]
    ['d4    #b0000000000010000]
    ['d5    #b0000000000100000]
    ['d6    #b0000000001000000]
    ['d7    #b0000000010000000]
    ['a0    #b0000000100000000]
    ['a1    #b0000001000000000]
    ['a2    #b0000010000000000]
    ['a3    #b0000100000000000]
    ['a4    #b0001000000000000]
    ['a5    #b0010000000000000]
    ['a6    #b0100000000000000]
    ['a7    #b1000000000000000]
    ))
(define (68kreglist-mask-posinc l)
  (for/fold
      ([mask 0])
      ([item l])
    (|| mask (68kreglist-maskitem-posinc item))))
(define 68kreglist-maskitem-predec
  (match-lambda
    ['a7 #b0000000000000001]
    ['a6 #b0000000000000010]
    ['a5 #b0000000000000100]
    ['a4 #b0000000000001000]
    ['a3 #b0000000000010000]
    ['a2 #b0000000000100000]
    ['a1 #b0000000001000000]
    ['a0 #b0000000010000000]
    ['d7 #b0000000100000000]
    ['d6 #b0000001000000000]
    ['d5 #b0000010000000000]
    ['d4 #b0000100000000000]
    ['d3 #b0001000000000000]
    ['d2 #b0010000000000000]
    ['d1 #b0100000000000000]
    ['d0 #b1000000000000000]))
(define (68kreglist-mask-predec l)
  (for/fold
      ([mask 0])
      ([item l])
    (|| mask (68kreglist-maskitem-predec item))))
        

(define 68kreg-dest-num
  (match-lambda
    ['d0 #b000000000000]
    ['d1 #b001000000000]
    ['d2 #b010000000000]
    ['d3 #b011000000000]
    ['d4 #b100000000000]
    ['d5 #b101000000000]
    ['d6 #b110000000000]
    ['d7 #b111000000000]
    
    ['a0 #b000000000000]
    ['a1 #b001000000000]
    ['a2 #b010000000000]
    ['a3 #b011000000000]
    ['a4 #b100000000000]
    ['a5 #b101000000000]
    ['a6 #b110000000000]
    ['a7 #b111000000000]
    ['sp #b111000000000]
    ))
(define 68kreg-mode+num
  (match-lambda
    ['d0 #b000000]
    ['d1 #b000001]
    ['d2 #b000010]
    ['d3 #b000011]
    ['d4 #b000100]
    ['d5 #b000101]
    ['d6 #b000110]
    ['d7 #b000111]
    
    ['a0 #b001000]
    ['a1 #b001001]
    ['a2 #b001010]
    ['a3 #b001011]
    ['a4 #b001100]
    ['a5 #b001101]
    ['a6 #b001110]
    ['a7 #b001111]
    ['sp #b001111]
    
    ['(a0+) #b010000]
    ['(a1+) #b010001]
    ['(a2+) #b010010]
    ['(a3+) #b010011]
    ['(a4+) #b010100]
    ['(a5+) #b010101]
    ['(a6+) #b010110]
    ['(a7+) #b010111]
    ['(sp+) #b010111]
    
    ['(-a0) #b011000]
    ['(-a1) #b011001]
    ['(-a2) #b011010]
    ['(-a3) #b011011]
    ['(-a4) #b011100]
    ['(-a5) #b011101]
    ['(-a6) #b011110]
    ['(-a7) #b011111]
    ['(-sp) #b011111]
    
    [`(,(? 68kimm.l? disp) ,(? 68kareg? a))                   (+ (68kreg-mode+num a) #b100000)]
    [`(,(? 68kimm.l? disp) ,(? 68kareg? a))                   (+ (68kreg-mode+num a) #b100000)]
    [`(,(? 68kdisp.s? disp) ,(? 68kareg? a) ,(? 68kreg.s? x)) (+ (68kreg-mode+num a) #b101000)]
    [`(,(? 68kdisp.w? disp) pc)                               #b111010]
    [`(,(? 68kdisp.s? disp) pc ,(? 68kreg.s? x))              #b111011]
    [`(w ,(? 68kaddr.w? addr))                                #b111000]
    [`(l ,(? 68kaddr.l? addr))                                #b111001]
    [(? 68kimm.w? i)                                          #b111100]
    ))

(define (68kreg-brief-ext-word disp reg)
  (+ (68krel.s disp)
     (match reg
       ['d0.w #x0000] ['d1.w #x1000] ['d2.w #x2000] ['d3.w #x3000]
       ['d4.w #x4000] ['d5.w #x5000] ['d6.w #x6000] ['d7.w #x7000]
       ['d0.l #x0800] ['d1.l #x1800] ['d2.l #x2800] ['d3.l #x3800]
       ['d4.l #x4800] ['d5.l #x5800] ['d6.l #x6800] ['d7.l #x7800]
       ['a0.w #x8000] ['a1.w #x9000] ['a2.w #xa000] ['a3.w #xb000]
       ['a4.w #xc000] ['a5.w #xd000] ['a6.w #xe000] ['a7.w #xf000] ['sp.w #xf000]
       ['a0.l #x8800] ['a1.l #x9800] ['a2.l #xa800] ['a3.l #xb800]
       ['a4.l #xc800] ['a5.l #xd800] ['a6.l #xe800] ['a7.l #xf800] ['sp.l #xf800]
       )))
(define 68kreg-extra-words
  (match-lambda
    [(? 68kdreg? d) '()]
    [(? 68kareg? a) '()]
    [(? 68kareg.posinc? a) '()]
    [(? 68kareg.predec? a) '()]
    [`(,(? 68kimm.w? disp) ,(? 68kareg? a)) `(,disp)]
    [`(,(? 68kimm.b? disp) ,(? 68kareg? a) ,(? 68kreg.s? b)) `(,(68kreg-brief-ext-word disp b))]
    [`(,(? 68krel.w? disp) pc) `(, (68krel.w disp))]
    [`(,(? 68krel.s? disp) pc ,(? 68kreg.s? b)) `(,(68kreg-brief-ext-word disp b))] ; watch this one
    [`(w ,(? 68kaddr.w? addr)) `(,addr)]
    [`(l ,(? 68kaddr.l? addr)) `(,(asm-w 1 addr) ,(asm-w 0 addr))]
    [(? 68kimm.w? imm) `(,imm)]
    ))

(define 68kdreg-num2
  (match-lambda
    ['d0 #b0000000000000000]
    ['d1 #b0000001000000000]
    ['d2 #b0000010000000000]
    ['d3 #b0000011000000000]
    ['d4 #b0000100000000000]
    ['d5 #b0000101000000000]
    ['d6 #b0000110000000000]
    ['d7 #b0000111000000000]))
(define 68kareg-num2
  (match-lambda
    ['a0 #b0000000000000000]
    ['a1 #b0000001000000000]
    ['a2 #b0000010000000000]
    ['a3 #b0000011000000000]
    ['a4 #b0000100000000000]
    ['a5 #b0000101000000000]
    ['a6 #b0000110000000000]
    ['a7 #b0000111000000000]
    ['sp #b0000111000000000]))
(define 68kreg-mode+num2
  (match-lambda
    [(? 68kdreg? d) (68kdreg-num2 d)]
    [(? 68kareg? a) (+ #b001000000 (68kareg-num2 a))]
    [`(,(? 68kimm.l? disp) ,(? 68kareg? a))                   (+ (68kreg-mode+num2 a) #b100000000)]
    [`(,(? 68kimm.l? disp) ,(? 68kareg? a))                   (+ (68kreg-mode+num2 a) #b100000000)]
    [`(,(? 68kdisp.s? disp) ,(? 68kareg? a) ,(? 68kreg.s? x)) (+ (68kreg-mode+num2 a) #b101000000)]
    [`(,(? 68kdisp.w? disp) pc)                               #b010111000000]
    [`(,(? 68kdisp.s? disp) pc ,(? 68kreg.s? x))              #b011111000000]
    [`(w ,(? 68kaddr.w? addr))                                #b000111000000]
    [`(l ,(? 68kaddr.l? addr))                                #b001111000000]
    [(? 68kimm.w? i)                                          #b100111000000]))
    
(define 68kreg?
  (match-lambda
        [(? 68kdreg? _) #t]
        [(? 68kareg? _) #t]
        [(? 68kareg.posinc? r) #t]
        [(? 68kareg.predec? r) #t]
        [`(,(? 68kdisp.w? _) ,(? 68kareg? _))                  #t]
        [`(,(? 68kdisp.s? _) ,(? 68kareg? _) ,(? 68kreg.s? _)) #t]
        [`(,(? 68kdisp.w? _) pc)                               #t]
        [`(,(? 68kdisp.w? _) pc ,(? 68kreg.s? _))              #t]
        [`(w ,(? 68kaddr.w? _)) #t]
        [`(l ,(? 68kaddr.l? _)) #t]
        [(? 68kimm.w? _) #t]
        ))
(make-architecture
 '68000 #t
 (match-lambda
   [`(illegal) (dw #b0100101011111100)]
   [`(reset)   (dw #b0100111001110000)]
   [`(nop)     (dw #b0100111001110001)]
   [`(rte)     (dw #b0100111001110011)]
   [`(rts)     (dw #b0100111001110101)]
   [`(trapv)   (dw #b0100111001110110)]
   [`(rtr)     (dw #b0100111001110111)]
   
   
   [`(ori ,(? 68kimm.b? imm) ccr)   (dw #b0000000000111100 imm)]
   [`(ori ,(? 68kimm.w? imm) sr)    (dw #b0000000001111100 imm)]
   [`(andi ,(? 68kimm.b? imm) ccr)  (dw #b0000001000111100 imm)]
   [`(andi ,(? 68kimm.w? imm) sr)   (dw #b0000001001111100 imm)]
   [`(eori ,(? 68kimm.b? imm) ccr)  (dw #b0000101000111100 imm)]
   [`(eori ,(? 68kimm.w? imm) sr)   (dw #b0000101001111100 imm)]
   [`(move.w sr ,(? 68kreg? reg))  (dw (+ #b0100000011000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(move.w ,(? 68kreg? reg) ccr) (dw (+ #b0100010011000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(move.w ,(? 68kreg? reg) sr)  (dw (+ #b0100011011000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   
   [`(ori.b  ,(? 68kreg? reg) ,(? 68kimm.b? imm)) (dw (+ #b0000000000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(ori.w  ,(? 68kreg? reg) ,(? 68kimm.w? imm)) (dw (+ #b0000000001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(ori.l  ,(? 68kreg? reg) ,(? 68kimm.l? imm)) (dw (+ #b0000000010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]
   
   [`(andi.b ,(? 68kreg? reg) ,(? 68kimm.b? imm)) (dw (+ #b0000001000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(andi.w ,(? 68kreg? reg) ,(? 68kimm.w? imm)) (dw (+ #b0000001001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(andi.l ,(? 68kreg? reg) ,(? 68kimm.l? imm)) (dw (+ #b0000001010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]

   [`(subi.b ,(? 68kreg? reg) ,(? 68kimm.b? imm)) (dw (+ #b0000010000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(subi.w ,(? 68kreg? reg) ,(? 68kimm.w? imm)) (dw (+ #b0000010001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(subi.l ,(? 68kreg? reg) ,(? 68kimm.l? imm)) (dw (+ #b0000010010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]
   
   [`(addi.b ,(? 68kreg? reg) ,(? 68kimm.b? imm)) (dw (+ #b0000011000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(addi.w ,(? 68kreg? reg) ,(? 68kimm.w? imm)) (dw (+ #b0000011001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(addi.l ,(? 68kreg? reg) ,(? 68kimm.l? imm)) (dw (+ #b0000011010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]

   [`(eori.b ,(? 68kreg? reg) ,(? 68kimm.b? imm)) (dw (+ #b0000101000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(eori.w ,(? 68kreg? reg) ,(? 68kimm.w? imm)) (dw (+ #b0000101001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(eori.l ,(? 68kreg? reg) ,(? 68kimm.l? imm)) (dw (+ #b0000101010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]
   
   [`(cmpi.b ,(? 68kreg? reg) ,(? 68kimm.b? imm)) (dw (+ #b0000110000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(cmpi.w ,(? 68kreg? reg) ,(? 68kimm.w? imm)) (dw (+ #b0000110001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(cmpi.l ,(? 68kreg? reg) ,(? 68kimm.l? imm)) (dw (+ #b0000110010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]
   
   [`(btst ,(? 68kreg? reg) ,(? 68kbitindex? imm))   (dw (+ #b0000100000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(btst ,(? 68kreg? src) ,(? 68kbitindex? imm) ,(? 68kdreg? dest))   (dw (+ #b0000000100000000 (68kreg-mode+num src) (68kdreg-num2 dest)) (68kreg-extra-words src) imm)]
   [`(bchg ,(? 68kreg? reg) ,(? 68kbitindex? imm))   (dw (+ #b0000100001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(bchg ,(? 68kreg? reg) ,(? 68kbitindex? imm) ,(? 68kdreg? dest))   (dw (+ #b0000100101000000 (68kreg-mode+num reg) (68kdreg-num2 dest)) (68kreg-extra-words reg) imm)]
   [`(bclr ,(? 68kreg? reg) ,(? 68kbitindex? imm))   (dw (+ #b0000100010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(bclr ,(? 68kreg? reg) ,(? 68kbitindex? imm) ,(? 68kdreg? dest))   (dw (+ #b0000100110000000 (68kreg-mode+num reg) (68kdreg-num2 dest)) (68kreg-extra-words reg) imm)]
   [`(bset ,(? 68kreg? reg) ,(? 68kbitindex? imm))   (dw (+ #b0000100011000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(bset ,(? 68kreg? reg) ,(? 68kbitindex? imm) ,(? 68kdreg? dest))   (dw (+ #b0000100111000000 (68kreg-mode+num reg) (68kdreg-num2 dest)) (68kreg-extra-words reg) imm)]
   
   [`(negx.b ,(? 68kreg? reg))                   (dw (+ #b0100000000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(negx.w ,(? 68kreg? reg))                   (dw (+ #b0100000001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(negx.l ,(? 68kreg? reg))                   (dw (+ #b0100000010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   
   [`(clr.b ,(? 68kreg? reg))                    (dw (+ #b0100001000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(clr.w ,(? 68kreg? reg))                    (dw (+ #b0100001001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(clr.l ,(? 68kreg? reg))                    (dw (+ #b0100001010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   
   [`(neg.b ,(? 68kreg? reg))                    (dw (+ #b0100010000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(neg.w ,(? 68kreg? reg))                    (dw (+ #b0100010001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(neg.l ,(? 68kreg? reg))                    (dw (+ #b0100010010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   
   [`(not.b ,(? 68kreg? reg))                    (dw (+ #b0100011000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(not.w ,(? 68kreg? reg))                    (dw (+ #b0100011001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(not.l ,(? 68kreg? reg))                    (dw (+ #b0100011010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   
   [`(bra.s ,(? 68krel.s? disp))                (dw (+ #b0110000000000000 (68krel.s disp)))]
   [`(bra.w ,(? 68krel.w? disp))                 (dw    #b0110000000000000 (68krel.w disp))]
   [`(bra.l ,(? 68krel.l? disp))                 (dw    #b0110000000000000 (68krel.l disp))]
   
   [`(bsr.s ,(? 68krel.s? disp))                (dw (+ #b0110000100000000 (68krel.s disp)))]
   [`(bsr.w ,(? 68krel.w? disp))                 (dw    #b0110000100000000 (68krel.w disp))]
   [`(bsr.l ,(? 68krel.l? disp))                 (dw    #b0110000111111111 (68krel.l disp))]
   
   [`(bhi.s ,(? 68krel.s? disp))                (dw (+ #b0110001000000000 (68krel.s disp)))]
   [`(bhi.w ,(? 68krel.w? disp))                 (dw    #b0110001000000000 (68krel.w disp))]
   [`(bhi.l ,(? 68krel.l? disp))                 (dw    #b0110001011111111 (68krel.l disp))]
   [`(bls.s ,(? 68krel.s? disp))                (dw (+ #b0110001100000000 (68krel.s disp)))]
   [`(bls.w ,(? 68krel.w? disp))                 (dw    #b0110001100000000 (68krel.w disp))]
   [`(bls.l ,(? 68krel.l? disp))                 (dw    #b0110001111111111 (68krel.l disp))]
   [`(bcc.s ,(? 68krel.s? disp))                (dw (+ #b0110010000000000 (68krel.s disp)))]
   [`(bcc.w ,(? 68krel.w? disp))                 (dw    #b0110010000000000 (68krel.w disp))]
   [`(bcc.l ,(? 68krel.l? disp))                 (dw    #b0110010011111111 (68krel.l disp))]
   [`(bcs.s ,(? 68krel.s? disp))                (dw (+ #b0110010100000000 (68krel.s disp)))]
   [`(bcs.w ,(? 68krel.w? disp))                 (dw    #b0110010100000000 (68krel.w disp))]
   [`(bcs.l ,(? 68krel.l? disp))                 (dw    #b0110010111111111 (68krel.l disp))]
   [`(bne.s ,(? 68krel.s? disp))                (dw (+ #b0110011000000000 (68krel.s disp)))]
   [`(bne.w ,(? 68krel.w? disp))                 (dw    #b0110011000000000 (68krel.w disp))]
   [`(bne.l ,(? 68krel.l? disp))                 (dw    #b0110011011111111 (68krel.l disp))]
   [`(beq.s ,(? 68krel.s? disp))                (dw (+ #b0110011100000000 (68krel.s disp)))]
   [`(beq.w ,(? 68krel.w? disp))                 (dw    #b0110011100000000 (68krel.w disp))]
   [`(beq.l ,(? 68krel.l? disp))                 (dw    #b0110011111111111 (68krel.l disp))]
   [`(bvc.s ,(? 68krel.s? disp))                (dw (+ #b0110100000000000 (68krel.s disp)))]
   [`(bvc.w ,(? 68krel.w? disp))                 (dw    #b0110100000000000 (68krel.w disp))]
   [`(bvc.l ,(? 68krel.l? disp))                 (dw    #b0110100011111111 (68krel.l disp))]
   [`(bvs.s ,(? 68krel.s? disp))                (dw (+ #b0110100100000000 (68krel.s disp)))]
   [`(bvs.w ,(? 68krel.w? disp))                 (dw    #b0110100100000000 (68krel.w disp))]
   [`(bvs.l ,(? 68krel.l? disp))                 (dw    #b0110100111111111 (68krel.l disp))]
   [`(bpl.s ,(? 68krel.s? disp))                (dw (+ #b0110101000000000 (68krel.s disp)))]
   [`(bpl.w ,(? 68krel.w? disp))                 (dw    #b0110101000000000 (68krel.w disp))]
   [`(bpl.l ,(? 68krel.l? disp))                 (dw    #b0110101011111111 (68krel.l disp))]
   [`(bmi.s ,(? 68krel.s? disp))                (dw (+ #b0110101100000000 (68krel.s disp)))]
   [`(bmi.w ,(? 68krel.w? disp))                 (dw    #b0110101100000000 (68krel.w disp))]
   [`(bmi.l ,(? 68krel.l? disp))                 (dw    #b0110101111111111 (68krel.l disp))]
   [`(bge.s ,(? 68krel.s? disp))                (dw (+ #b0110110000000000 (68krel.s disp)))]
   [`(bge.w ,(? 68krel.w? disp))                 (dw    #b0110110000000000 (68krel.w disp))]
   [`(bge.l ,(? 68krel.l? disp))                 (dw    #b0110110011111111 (68krel.l disp))]
   [`(blt.s ,(? 68krel.s? disp))                (dw (+ #b0110110100000000 (68krel.s disp)))]
   [`(blt.w ,(? 68krel.w? disp))                 (dw    #b0110110100000000 (68krel.w disp))]
   [`(blt.l ,(? 68krel.l? disp))                 (dw    #b0110110111111111 (68krel.l disp))]
   [`(bgt.s ,(? 68krel.s? disp))                (dw (+ #b0110111000000000 (68krel.s disp)))]
   [`(bgt.w ,(? 68krel.w? disp))                 (dw    #b0110111000000000 (68krel.w disp))]
   [`(bgt.l ,(? 68krel.l? disp))                 (dw    #b0110111011111111 (68krel.l disp))]
   [`(ble.s ,(? 68krel.s? disp))                (dw (+ #b0110111100000000 (68krel.s disp)))]
   [`(ble.w ,(? 68krel.w? disp))                 (dw    #b0110111100000000 (68krel.w disp))]
   [`(ble.l ,(? 68krel.l? disp))                 (dw    #b0110111111111111 (68krel.l disp))]

   [`(st  ,(? 68kreg? reg))                 (dw (+ #b0101000011000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(sf  ,(? 68kreg? reg))                 (dw (+ #b0101000111000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(shi ,(? 68kreg? reg))                 (dw (+ #b0101001011000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(sls ,(? 68kreg? reg))                 (dw (+ #b0101001111000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(scc ,(? 68kreg? reg))                 (dw (+ #b0101010011000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(scs ,(? 68kreg? reg))                 (dw (+ #b0101010111000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(sne ,(? 68kreg? reg))                 (dw (+ #b0101011011000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(seq ,(? 68kreg? reg))                 (dw (+ #b0101011111000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(svc ,(? 68kreg? reg))                 (dw (+ #b0101100011000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(svs ,(? 68kreg? reg))                 (dw (+ #b0101100111000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(spl ,(? 68kreg? reg))                 (dw (+ #b0101101011000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(smi ,(? 68kreg? reg))                 (dw (+ #b0101101111000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(sge ,(? 68kreg? reg))                 (dw (+ #b0101110011000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(slt ,(? 68kreg? reg))                 (dw (+ #b0101110111000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(sgt ,(? 68kreg? reg))                 (dw (+ #b0101111011000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(sle ,(? 68kreg? reg))                 (dw (+ #b0101111111000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]

   [`(dbt  ,(? 68kdreg? reg) ,(? 68krel.w? disp))                 (dw (+ #b0101000011001000 (68kreg-mode+num reg)) (68krel.w disp))]
   [`(dbf  ,(? 68kdreg? reg) ,(? 68krel.w? disp))                 (dw (+ #b0101000111001000 (68kreg-mode+num reg)) (68krel.w disp))]
   [`(dbhi ,(? 68kdreg? reg) ,(? 68krel.w? disp))                 (dw (+ #b0101001011001000 (68kreg-mode+num reg)) (68krel.w disp))]
   [`(dbls ,(? 68kdreg? reg) ,(? 68krel.w? disp))                 (dw (+ #b0101001111001000 (68kreg-mode+num reg)) (68krel.w disp))]
   [`(dbcc ,(? 68kdreg? reg) ,(? 68krel.w? disp))                 (dw (+ #b0101010011001000 (68kreg-mode+num reg)) (68krel.w disp))]
   [`(dbcs ,(? 68kdreg? reg) ,(? 68krel.w? disp))                 (dw (+ #b0101010111001000 (68kreg-mode+num reg)) (68krel.w disp))]
   [`(dbne ,(? 68kdreg? reg) ,(? 68krel.w? disp))                 (dw (+ #b0101011011001000 (68kreg-mode+num reg)) (68krel.w disp))]
   [`(dbeq ,(? 68kdreg? reg) ,(? 68krel.w? disp))                 (dw (+ #b0101011111001000 (68kreg-mode+num reg)) (68krel.w disp))]
   [`(dbvc ,(? 68kdreg? reg) ,(? 68krel.w? disp))                 (dw (+ #b0101100011001000 (68kreg-mode+num reg)) (68krel.w disp))]
   [`(dbvs ,(? 68kdreg? reg) ,(? 68krel.w? disp))                 (dw (+ #b0101100111001000 (68kreg-mode+num reg)) (68krel.w disp))]
   [`(dbpl ,(? 68kdreg? reg) ,(? 68krel.w? disp))                 (dw (+ #b0101101011001000 (68kreg-mode+num reg)) (68krel.w disp))]
   [`(dbmi ,(? 68kdreg? reg) ,(? 68krel.w? disp))                 (dw (+ #b0101101111001000 (68kreg-mode+num reg)) (68krel.w disp))]
   [`(dbge ,(? 68kdreg? reg) ,(? 68krel.w? disp))                 (dw (+ #b0101110011001000 (68kreg-mode+num reg)) (68krel.w disp))]
   [`(dblt ,(? 68kdreg? reg) ,(? 68krel.w? disp))                 (dw (+ #b0101110111001000 (68kreg-mode+num reg)) (68krel.w disp))]
   [`(dbgt ,(? 68kdreg? reg) ,(? 68krel.w? disp))                 (dw (+ #b0101111011001000 (68kreg-mode+num reg)) (68krel.w disp))]
   [`(dble ,(? 68kdreg? reg) ,(? 68krel.w? disp))                 (dw (+ #b0101111111001000 (68kreg-mode+num reg)) (68krel.w disp))]
   
   [`(movea.b ,(? 68kreg? src) ,(? 68kareg? dst))  (dw (+ #b0001000001000000 (68kreg-mode+num src) (68kareg-num2 dst)) (68kreg-extra-words src))]
   [`(movea.w ,(? 68kreg? src) ,(? 68kareg? dst))  (dw (+ #b0011000001000000 (68kreg-mode+num src) (68kareg-num2 dst)) (68kreg-extra-words src))]
   [`(movea.l ,(? 68kreg? src) ,(? 68kareg? dst))  (dw (+ #b0010000001000000 (68kreg-mode+num src) (68kareg-num2 dst)) (68kreg-extra-words src))]
   
   [`(move.b ,(? 68kreg? src) ,(? 68kreg? dst))    (dw (+ #b0001000000000000 (68kreg-mode+num src) (68kreg-mode+num2 dst)) (68kreg-extra-words src) (68kreg-extra-words dst))]
   [`(move.w ,(? 68kreg? src) ,(? 68kreg? dst))    (dw (+ #b0011000000000000 (68kreg-mode+num src) (68kreg-mode+num2 dst)) (68kreg-extra-words src) (68kreg-extra-words dst))]
   [`(move.l ,(? 68kreg? src) ,(? 68kreg? dst))    (dw (+ #b0010000000000000 (68kreg-mode+num src) (68kreg-mode+num2 dst)) (68kreg-extra-words src) (68kreg-extra-words dst))]
   
   [`(moveq ,(? 68kimm.b? byte) ,(? 68kdreg? dst))  (dw (+ #b0111000000000000 (and byte #xff) (68kdreg-num2 dst)))]
   
   [`(movem.w ,(? 68kreglist? rlist) ,(? 68kareg.posinc? dst)) (dw (+ #b0100100010000000 (68kreg-mode+num dst)) (68kreg-extra-words dst) (68kreglist-mask-posinc rlist))]
   [`(movem.l ,(? 68kreglist? rlist) ,(? 68kareg.posinc? dst)) (dw (+ #b0100100011000000 (68kreg-mode+num dst)) (68kreg-extra-words dst) (68kreglist-mask-posinc rlist))]
   [`(movem.w ,(? 68kreglist? rlist) ,(? 68kareg.predec? dst)) (dw (+ #b0100100010000000 (68kreg-mode+num dst)) (68kreg-extra-words dst) (68kreglist-mask-predec rlist))]
   [`(movem.l ,(? 68kreglist? rlist) ,(? 68kareg.predec? dst)) (dw (+ #b0100100011000000 (68kreg-mode+num dst)) (68kreg-extra-words dst) (68kreglist-mask-predec rlist))]
   
   [`(movem.w ,(? 68kareg.posinc? src) ,(? 68kreglist? rlist)) (dw (+ #b0100110010000000 (68kreg-mode+num src)) (68kreg-extra-words src) (68kreglist-mask-posinc rlist))]
   [`(movem.l ,(? 68kareg.posinc? src) ,(? 68kreglist? rlist)) (dw (+ #b0100110011000000 (68kreg-mode+num src)) (68kreg-extra-words src) (68kreglist-mask-posinc rlist))]
   [`(movem.w ,(? 68kareg.predec? src) ,(? 68kreglist? rlist)) (dw (+ #b0100110010000000 (68kreg-mode+num src)) (68kreg-extra-words src) (68kreglist-mask-predec rlist))]
   [`(movem.l ,(? 68kareg.predec? src) ,(? 68kreglist? rlist)) (dw (+ #b0100110011000000 (68kreg-mode+num src)) (68kreg-extra-words src) (68kreglist-mask-predec rlist))]
   
   [`(lea ,(? 68kreg? src) ,(? 68kareg? dst)) (dw (+ #b0100000111000000 (68kreg-mode+num src) (68kareg-num2 dst)) (68kreg-extra-words src))]
   
   
   ))
   
