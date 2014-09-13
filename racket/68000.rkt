#lang racket

(require "plasm.rkt")

(define (68kimmb? n)
  (and (>= n -128)
       (<= n 255)))

(define (68kimmw? n)
  (and (>= n -32768)
       (<= n 65535)))

(define (68kimml? n)
  (and (>= n (- #x80000000))
       (<= n #xffffffff)))

(define (68kdispw? n)
  (and (>= n -32768)
       (<= n 32767)))

(define (68kaddrw? n)
  (and (>= n 0)
       (<= n 65535)))

(define (68kaddrl? n)
  (and (>= n 0)
       (<= n #xffffffff)))

(define (68kdispb? n)
  (and (>= n (- #x80))
       (<= n #x7f)))

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
    
    [`(,(? 68kimml? disp) ,(? 68kareg? a))                   (+ (68kreg-mode+num a) #b100000)]
    [`(,(? 68kimml? disp) ,(? 68kareg? a))                   (+ (68kreg-mode+num a) #b100000)]
    [`(,(? 68kdispb? disp) ,(? 68kareg? a) ,(? 68kreg.s? x)) (+ (68kreg-mode+num a) #b101000)]
    [`(,(? 68kdispw? disp) pc)                               #b111010]
    [`(,(? 68kdispb? disp) pc ,(? 68kreg.s? x))              #b111011]
    [`(w ,(? 68kaddrw? addr))                                #b111000]
    [`(l ,(? 68kaddrl? addr))                                #b111001]
    [(? 68kimmw? i)                                          #b111100]
    ))

(define (68kreg-brief-ext-word disp reg)
  (+ (bitwise-and #x255 disp)
     (match reg
            ['d0.w #x0000]
            ['d1.w #x1000]
            ['d2.w #x2000]
            ['d3.w #x3000]
            ['d4.w #x4000]
            ['d5.w #x5000]
            ['d6.w #x6000]
            ['d7.w #x7000]
            ['d0.l #x0800]
            ['d1.l #x1800]
            ['d2.l #x2800]
            ['d3.l #x3800]
            ['d4.l #x4800]
            ['d5.l #x5800]
            ['d6.l #x6800]
            ['d7.l #x7800]
            ['a0.w #x8000]
            ['a1.w #x9000]
            ['a2.w #xa000]
            ['a3.w #xb000]
            ['a4.w #xc000]
            ['a5.w #xd000]
            ['a6.w #xe000]
            ['a7.w #xf000]
            ['sp.w #xf000]
            ['a0.l #x8800]
            ['a1.l #x9800]
            ['a2.l #xa800]
            ['a3.l #xb800]
            ['a4.l #xc800]
            ['a5.l #xd800]
            ['a6.l #xe800]
            ['a7.l #xf800]
            ['sp.l #xf800]
            )))
(define 68kreg-extra-words
  (match-lambda
    [(? 68kdreg? d) '()]
    [(? 68kareg? a) '()]
    [(? 68kareg.posinc? a) '()]
    [(? 68kareg.predec? a) '()]
    [`(,(? 68kimmw? disp) ,(? 68kareg? a)) `(,disp)]
    [`(,(? 68kimmb? disp) ,(? 68kareg? a) ,(? 68kreg.s? b)) `(,(68kreg-brief-ext-word disp b))]
    [`(,(? 68kimmw? disp) pc) `(,disp)]
    [`(,(? 68kimmw? disp) pc ,(? 68kreg.s? b)) `(,(68kreg-brief-ext-word disp b))]
    [`(w ,(? 68kaddrw? addr)) `(,addr)]
    [`(l ,(? 68kaddrl? addr)) `(,(asm-w 1 addr) ,(asm-w 0 addr))]
    [(? 68kimmw? imm) `(,imm)]
    ))
    
(define 68kreg?
  (match-lambda
        [(? 68kdreg? _) #t]
        [(? 68kareg? _) #t]
        [(? 68kareg.posinc? r) #t]
        [(? 68kareg.predec? r) #t]
        [`(,(? 68kdispw? _) ,(? 68kareg? _))                  #t]
        [`(,(? 68kdispb? _) ,(? 68kareg? _) ,(? 68kreg.s? _)) #t]
        [`(,(? 68kdispw? _) pc)                               #t]
        [`(,(? 68kdispw? _) pc ,(? 68kreg.s? _))              #t]
        [`(w ,(? 68kaddrw? _)) #t]
        [`(l ,(? 68kaddrl? _)) #t]
        [(? 68kimmw? _) #t]
        ))
(architecture
 '68000 #t
 (match-lambda
   [`(illegal) (dw #b0100101011111100)]
   [`(reset)   (dw #b0100111001110000)]
   [`(nop)     (dw #b0100111001110001)]
   [`(rte)     (dw #b0100111001110011)]
   [`(rts)     (dw #b0100111001110101)]
   [`(trapv)   (dw #b0100111001110110)]
   [`(rtr)     (dw #b0100111001110111)]
   
   
   [`(ori ccr ,(? 68kimmb? imm))  (dw #b0000000000111100 imm)]
   [`(ori sr ,(? 68kimmw? imm))   (dw #b0000000001111100 imm)]
   [`(andi ccr ,(? 68kimmb? imm)) (dw #b0000001000111100 imm)]
   [`(andi sr ,(? 68kimmw? imm))  (dw #b0000001001111100 imm)]
   [`(eori ccr ,(? 68kimmb? imm)) (dw #b0000101000111100 imm)]
   [`(eori sr ,(? 68kimmw? imm))  (dw #b0000101001111100 imm)]
   
   [`(ori.b  ,(? 68kreg? reg) ,(? 68kimmb? imm)) (dw (+ #b0000000000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(ori.w  ,(? 68kreg? reg) ,(? 68kimmw? imm)) (dw (+ #b0000000001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(ori.l  ,(? 68kreg? reg) ,(? 68kimml? imm)) (dw (+ #b0000000010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]
   
   [`(andi.b ,(? 68kreg? reg) ,(? 68kimmb? imm)) (dw (+ #b0000001000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(andi.w ,(? 68kreg? reg) ,(? 68kimmw? imm)) (dw (+ #b0000001001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(andi.l ,(? 68kreg? reg) ,(? 68kimml? imm)) (dw (+ #b0000001010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]

   [`(subi.b ,(? 68kreg? reg) ,(? 68kimmb? imm)) (dw (+ #b0000010000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(subi.w ,(? 68kreg? reg) ,(? 68kimmw? imm)) (dw (+ #b0000010001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(subi.l ,(? 68kreg? reg) ,(? 68kimml? imm)) (dw (+ #b0000010010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]
   
   [`(addi.b ,(? 68kreg? reg) ,(? 68kimmb? imm)) (dw (+ #b0000011000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(addi.w ,(? 68kreg? reg) ,(? 68kimmw? imm)) (dw (+ #b0000011001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(addi.l ,(? 68kreg? reg) ,(? 68kimml? imm)) (dw (+ #b0000011010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]

   [`(eori.b ,(? 68kreg? reg) ,(? 68kimmb? imm)) (dw (+ #b0000101000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(eori.w ,(? 68kreg? reg) ,(? 68kimmw? imm)) (dw (+ #b0000101001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(eori.l ,(? 68kreg? reg) ,(? 68kimml? imm)) (dw (+ #b0000101010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]
   
   [`(cmpi.b ,(? 68kreg? reg) ,(? 68kimmb? imm)) (dw (+ #b0000110000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(cmpi.w ,(? 68kreg? reg) ,(? 68kimmw? imm)) (dw (+ #b0000110001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(cmpi.l ,(? 68kreg? reg) ,(? 68kimml? imm)) (dw (+ #b0000110010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]
   
   [`(btst ,(? 68kreg? reg) ,(? 68kimmb? imm))   (dw (+ #b0000100000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(bchg ,(? 68kreg? reg) ,(? 68kimmb? imm))   (dw (+ #b0000100001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(bclr ,(? 68kreg? reg) ,(? 68kimmb? imm))   (dw (+ #b0000100010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(bset ,(? 68kreg? reg) ,(? 68kimmb? imm))   (dw (+ #b0000100011000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   
   [`(negx.b ,(? 68kreg? reg))                   (dw (+ #b0100000000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(negx.w ,(? 68kreg? reg))                   (dw (+ #b0100000001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(negx.l ,(? 68kreg? reg))                   (dw (+ #b0100000010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   
   [`(clr.b ,(? 68kreg? reg))                    (dw (+ #b0100001000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(clr.w ,(? 68kreg? reg))                    (dw (+ #b0100001001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   [`(clr.l ,(? 68kreg? reg))                    (dw (+ #b0100001010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg))]
   ))
   
