#lang racket

(require "plasm.rkt")

(define (68kimmb? n)
  (and (>= n -128)
       (<= n 255)))

(define (68kimmw? n)
  (and (>= n -32768)
       (<= n 65535)))

(define (68kimmd? n)
  (and (>= n (- #x80000000))
       (<= n #xffffffff)))


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
    ['a0 #b000000]
    ['a1 #b000001]
    ['a2 #b000010]
    ['a3 #b000011]
    ['a4 #b000100]
    ['a5 #b000101]
    ['a6 #b000110]
    ['a7 #b000111]
    
    ['d0 #b001000]
    ['d1 #b001001]
    ['d2 #b001010]
    ['d3 #b001011]
    ['d4 #b001100]
    ['d5 #b001101]
    ['d6 #b001110]
    ['d7 #b001111]
    
    ['(a0+) #b010000]
    ['(a1+) #b010001]
    ['(a2+) #b010010]
    ['(a3+) #b010011]
    ['(a4+) #b010100]
    ['(a5+) #b010101]
    ['(a6+) #b010110]
    ['(a7+) #b010111]
    
    ['(-a0) #b011000]
    ['(-a1) #b011001]
    ['(-a2) #b011010]
    ['(-a3) #b011011]
    ['(-a4) #b011100]
    ['(-a5) #b011101]
    ['(-a6) #b011110]
    ['(-a7) #b011111]
    [`(,(? 68kimmd? disp) ,(? 68kareg? a)) (+ (68kreg-mode+num a) #b100000)]
    [`(,(? 68kimmd? disp) ,(? 68kareg? a)) (+ (68kreg-mode+num a) #b100000)]
    [`(,(? 68kdispb? disp) ,(? 68kareg? a) ,(? 68kareg? x)) (+ (68kreg-mode+num a) #b101000)]
    [`(,(? 68kdispb? disp) ,(? 68kareg? a) ,(? 68kdreg? x)) (+ (68kreg-mode+num a) #b101000)]
    ))

(define 68kreg-extra-words
  (match-lambda
    [(? 68kdreg? d) '()]
    [(? 68kareg? a) '()]
    [(? 68kareg.posinc? a) '()]
    [(? 68kareg.predec? a) '()]
    [`(,(? 68kimmd? disp) ,(? 68kareg? a)) `(,disp)]
    
    ))
    
(define (68kreg? r)
  (or (68kdreg? r)
      (68kareg? r)
      (68kareg.posinc? r)
      (68kareg.predec? r)))
(architecture
 '68000 #t
 (match-lambda
   [`(ori ccr)  (dw #b0000000000111100)]
   [`(ori sr)   (dw #b0000000001111100)]
   [`(andi ccr) (dw #b0000001000111100)]
   [`(andi sr)  (dw #b0000001001111100)]
   [`(eori ccr) (dw #b0000101000111100)]
   [`(eori sr)  (dw #b0000101001111100)]
   
   [`(ori.b  ,(? 68kreg? reg) ,(? 68kimmb? imm)) (dw (+ #b0000000000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(ori.w  ,(? 68kreg? reg) ,(? 68kimmw? imm)) (dw (+ #b0000000001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(ori.d  ,(? 68kreg? reg) ,(? 68kimmd? imm)) (dw (+ #b0000000010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]
   
   [`(andi.b ,(? 68kreg? reg) ,(? 68kimmb? imm)) (dw (+ #b0000001000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(andi.w ,(? 68kreg? reg) ,(? 68kimmw? imm)) (dw (+ #b0000001001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(andi.d ,(? 68kreg? reg) ,(? 68kimmd? imm)) (dw (+ #b0000001010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]

   [`(subi.b ,(? 68kreg? reg) ,(? 68kimmb? imm)) (dw (+ #b0000010000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(subi.w ,(? 68kreg? reg) ,(? 68kimmw? imm)) (dw (+ #b0000010001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(subi.d ,(? 68kreg? reg) ,(? 68kimmd? imm)) (dw (+ #b0000010010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]
   
   [`(addi.b ,(? 68kreg? reg) ,(? 68kimmb? imm)) (dw (+ #b0000011000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(addi.w ,(? 68kreg? reg) ,(? 68kimmw? imm)) (dw (+ #b0000011001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(addi.d ,(? 68kreg? reg) ,(? 68kimmd? imm)) (dw (+ #b0000011010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]

   [`(eori.b ,(? 68kreg? reg) ,(? 68kimmb? imm)) (dw (+ #b0000101000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(eori.w ,(? 68kreg? reg) ,(? 68kimmw? imm)) (dw (+ #b0000101001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(eori.d ,(? 68kreg? reg) ,(? 68kimmd? imm)) (dw (+ #b0000101010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]
   
   [`(cmpi.b ,(? 68kreg? reg) ,(? 68kimmb? imm)) (dw (+ #b0000110000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(cmpi.w ,(? 68kreg? reg) ,(? 68kimmw? imm)) (dw (+ #b0000110001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(cmpi.d ,(? 68kreg? reg) ,(? 68kimmd? imm)) (dw (+ #b0000110010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) (asm-w 1 imm) (asm-w 0 imm))]
   
   [`(btst ,(? 68kreg? reg) ,(? 68kimmb? imm))   (dw (+ #b0000100000000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(bchg ,(? 68kreg? reg) ,(? 68kimmb? imm))   (dw (+ #b0000100001000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(bclr ,(? 68kreg? reg) ,(? 68kimmb? imm))   (dw (+ #b0000100010000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   [`(bset ,(? 68kreg? reg) ,(? 68kimmb? imm))   (dw (+ #b0000100011000000 (68kreg-mode+num reg)) (68kreg-extra-words reg) imm)]
   
   ))
   
