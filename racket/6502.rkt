#lang racket

(require "plasm.rkt")

(define (65imm? n)
  (number? n))
(define (65addr? n)
  (number? n))
(define (65rel? n)
  (number? n))

(define (65imm op n)
  (db op n))
(define (65addr op n)
  (db op (asm-b 0 n) (asm-b 1 n)))
(define (65rel op n)
  (db op (@@ n)))

(architecture
 '6502 #f
  (match-lambda
    [`(adc ,(? 65imm? n))        (65imm #x65 n)]
    [`(adc $,(? 65imm? n))       (65imm #x69 n)]
    [`(adc (,(? 65imm? n) x))    (65imm #x61 n)]
    [`(adc (,(? 65imm? n)) y)    (65imm #x71 n)]
    [`(adc (,(? 65imm? n)))      (65imm #x72 n)]
    [`(adc ,(? 65imm? n) x)      (65imm #x75 n)]
    [`(adc @,(? 65addr? n) x)    (65addr #x7D n)]
    [`(adc @,(? 65addr? n) y)    (65addr #x79 n)]
    [`(adc @,(? 65addr? n))      (65addr #x6D n)]
    
    [`(and ,(? 65imm? n))        (65imm #x25 n)]
    [`(and $,(? 65imm? n))       (65imm #x29 n)]
    [`(and (,(? 65imm? n) x))    (65imm #x21 n)]
    [`(and (,(? 65imm? n)) y)    (65imm #x31 n)]
    [`(and (,(? 65imm? n)))      (65imm #x32 n)]
    [`(and ,(? 65imm? n) x)      (65imm #x35 n)]
    [`(and @,(? 65addr? n) x)    (65addr #x3D n)]
    [`(and @,(? 65addr? n) y)    (65addr #x39 n)]
    [`(and @,(? 65addr? n))      (65addr #x2D n)]
    
    [`(asl a)                    (db #x0a)]
    [`(asl ,(? 65imm? n) x)      (db #x16 n)]
    [`(asl @,(? 65addr? n) x)    (65addr #x1e n)]
    [`(asl ,(? 65imm? n))        (db #x06 n)]
    [`(asl @,(? 65addr? n))      (65addr #x0e n)]
    
    [`(asl)                      (db #x0a)]
    
    [`(bcc ,(? 65rel? n))        (65rel #x90 n)]
    [`(bcs ,(? 65rel? n))        (65rel #xb0 n)]
    [`(beq ,(? 65rel? n))        (65rel #xf0 n)]
    [`(bmi ,(? 65rel? n))        (65rel #x30 n)]
    [`(bne ,(? 65rel? n))        (65rel #xd0 n)]
    [`(bpl ,(? 65rel? n))        (65rel #x10 n)]
    [`(bvc ,(? 65rel? n))        (65rel #x50 n)]
    [`(bvs ,(? 65rel? n))        (65rel #x70 n)]
    [`(bra ,(? 65rel? n))        (65rel #x80 n)]
    
    [`(bit ,(? 65imm? n))        (65imm #x24 n)]
    [`(bit $,(? 65imm? n))       (65imm #x89 n)]
    [`(bit (,(? 65imm? n)))      (65imm #x32 n)]
    [`(bit ,(? 65imm? n) x)      (65imm #x34 n)]
    [`(bit @,(? 65addr? n) x)    (65addr #x3C n)]
    [`(bit @,(? 65addr? n))      (65addr #x2C n)]

    [`(brk ,(? 65imm? n))        (65imm #x00 n)]
    [`(brk)                      (db 0)]

    [`(clc)                      (db #x18)]
    [`(cld)                      (db #xd8)]
    [`(cli)                      (db #x58)]
    [`(clv)                      (db #x68)]
    
    [`(cmp $,(? 65imm? n))       (65imm #xc9 n)]
    [`(cmp (,(? 65imm? n) x))    (65imm #xc1 n)]
    [`(cmp (,(? 65imm? n)) y)    (65imm #xd1 n)]
    [`(cmp (,(? 65imm? n)))      (65imm #xd2 n)]
    [`(cmp ,(? 65imm? n) x)      (65imm #xd5 n)]
    [`(cmp ,(? 65addr? n) x)     (65addr #xd0 n)]
    [`(cmp ,(? 65addr? n) y)     (65addr #xd9 n)]
    [`(cmp ,(? 65imm? n))        (65imm #xc5 n)]
    [`(cmp @,(? 65addr? n))      (65addr #xcd n)]
    
    [`(cpx $,(? 65imm? n))       (65imm #xe0 n)]
    [`(cpx ,(? 65imm? n))        (65imm #xe4 n)]
    [`(cpx @,(? 65addr? n))      (65addr #xec n)]
    
    [`(cpy $,(? 65imm? n))       (65imm #xc0 n)]
    [`(cpy ,(? 65imm? n))        (65imm #xc4 n)]
    [`(cpy @,(? 65addr? n))      (65addr #xcc n)]
    
    [`(dec ,(? 65imm? n) x)      (65imm #xd6 n)]
    [`(dec @,(? 65addr? n) x)    (65addr #xde n)]
    [`(dec ,(? 65imm? n))        (65imm #xc6 n)]
    [`(dec @,(? 65addr? n))      (65addr #xce n)]
    
    [`(dea)                      (db #x3a)]
    [`(dex)                      (db #xca)]
    [`(dey)                      (db #x88)]
    
    [`(eor $,(? 65imm? n))       (65imm #x49 n)]
    [`(eor (,(? 65imm? n) x))    (65imm #x41 n)]
    [`(eor (,(? 65imm? n)) y)    (65imm #x51 n)]
    [`(eor (,(? 65imm? n)))      (65imm #x52 n)]
    [`(eor ,(? 65imm? n) x)      (65imm #x55 n)]
    [`(eor ,(? 65addr? n) x)     (65addr #x50 n)]
    [`(eor ,(? 65addr? n) y)     (65addr #x59 n)]
    [`(eor ,(? 65imm? n))        (65imm #x45 n)]
    [`(eor @,(? 65addr? n))      (65addr #x4d n)]
    
    [`(inc ,(? 65imm? n) x)      (65imm #xf6 n)]
    [`(inc @,(? 65addr? n) x)    (65addr #xfe n)]
    [`(inc ,(? 65imm? n))        (65imm #xe6 n)]
    [`(inc @,(? 65addr? n))      (65addr #xee n)]
    
    [`(ina)                      (db #x1a)]
    [`(inx)                      (db #xe8)]
    [`(iny)                      (db #xc8)]
    
    [`(jmp (,(? 65addr? n) x))   (65addr #x7c n)]
    [`(jmp (,(? 65addr? n)))     (65addr #x6c n)]
    [`(jmp ,(? 65addr? n))       (65addr #x4c n)]
    
    [`(jsr ,(? 65addr? n))       (65addr #x20 n)]
    ))
