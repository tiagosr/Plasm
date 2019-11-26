#lang racket

(require "plasm.rkt")

(define (avr-reg-lo? r)
  (in-list? r '(r0 r1 r2 r3 r4 r5 r6 r7
                r8 r9 r10 r11 r12 r13 r14 r15)))
(define (avr-reg-hi? r)
  (in-list? r '(r16 r17 r18 r19 r20 r21 r22 r23
                r24 r25 r26 r27 r28 r29 r30 r31)))
(define (avr-reg-pair-24-30? r)
  (in-list? r '(r25:r24 r27:r26 r29:r28 r31:r30)))

(define (avr-reg? r)
  (or (avr-reg-lo? r) (avr-reg-hi? r)))

(define (avr-reg-s r)
  (list-ref 
   (assoc r '([r0 #x0]  [r1 #x1]  [r2 #x2]  [r3 #x3]
            [r4 #x4]  [r5 #x5]  [r6 #x6]  [r7 #x7]
            [r8 #x8]  [r9 #x9]  [r10 #xa] [r11 #xb]
            [r12 #xc] [r13 #xd] [r14 #xe] [r15 #xf]
            [r16 #x200] [r17 #x201] [r18 #x202] [r19 #x203]
            [r20 #x204] [r21 #x205] [r22 #x206] [r23 #x207]
            [r24 #x208] [r25 #x209] [r26 #x20a] [r27 #x20b]
            [r28 #x20c] [r29 #x20d] [r30 #x20e] [r31 #x20f])) 1))
(define (avr-reg-d r)
  (list-ref
   (assoc r '([r0 #x00]  [r1 #x10]  [r2 #x20]  [r3 #x30]
            [r4 #x40]  [r5 #x50]  [r6 #x60]  [r7 #x70]
            [r8 #x80]  [r9 #x90]  [r10 #xa0] [r11 #xb0]
            [r12 #xc0] [r13 #xd0] [r14 #xe0] [r15 #xf0]
            [r16 #x100] [r17 #x110] [r18 #x120] [r19 #x130]
            [r20 #x140] [r21 #x150] [r22 #x160] [r23 #x170]
            [r24 #x180] [r25 #x190] [r26 #x1a0] [r27 #x1b0]
            [r28 #x1c0] [r29 #x1d0] [r30 #x1e0] [r31 #x1f0])) 1))

(define (avr-reg-hi-d r)
  (list-ref
   (assoc r '([r16 #x00] [r17 #x10] [r18 #x20] [r19 #x30]
            [r20 #x40] [r21 #x50] [r22 #x60] [r23 #x70]
            [r24 #x80] [r25 #x90] [r26 #xa0] [r27 #xb0]
            [r28 #xc0] [r29 #xd0] [r30 #xe0] [r31 #xf0])) 1))
(define (avr-reg-pair-24-30 r)
  (list-ref
   (assoc r '([r25:r24 0] [r27:r26 1] [r29:r28 2] [r31:r30 3])) 1))

(define (avr-imm-6b? i) (between? 0 i 63))
(define (avr-imm-6b i) (+ (& #x0f i) (<< (& #x30 i) 2)))

(define (avr-imm-b? i) (between? -128 i 127))
(define (avr-imm-b i) (+ (& #x0f i) (<< (& #xf0 i) 4)))

(define (avr-bit? i) (between? 0 i 7))
(define (avr-bit-d i) (<< i 4))

; relative address is word-aligned
(define (avr-rel-7b? i) (between? -64 (>> (+ 2 (->@ i)) 1) 63))
(define (avr-rel-7b i) (<< (& #xfe (+ 2 (->@ i))) 2))




            
(make-architecture
 'avr8 #t
 (match-lambda
   [`(nop)      (dw      0)]
   [`(ret)      (dw #x9504)]
   [`(reti)     (dw #x9514)]
   [`(icall)    (dw #x9509)]
   [`(ijmp)     (dw #x9409)]
   [`(eicall)   (dw #x9519)]
   [`(eijmp)    (dw #x9419)]
   [`(sleep)    (dw #x9588)]
   [`(break)    (dw #x9598)]
   [`(wdr)      (dw #x95a8)]
   [`(spm)      (dw #x95e8)]
   
   [`(sec)      (dw #x9408)]
   [`(sez)      (dw #x9418)]
   [`(sen)      (dw #x9428)]
   [`(sev)      (dw #x9438)]
   [`(ses)      (dw #x9448)]
   [`(seh)      (dw #x9458)]
   [`(set)      (dw #x9468)]
   [`(sei)      (dw #x9478)]
   
   [`(clc)      (dw #x9488)]
   [`(clz)      (dw #x9498)]
   [`(cln)      (dw #x94a8)]
   [`(clv)      (dw #x94b8)]
   [`(cls)      (dw #x94c8)]
   [`(clh)      (dw #x94d8)]
   [`(clt)      (dw #x94e8)]
   [`(cli)      (dw #x94f8)]
   
   [`(add  ,(? avr-reg? src) ,(? avr-reg? dst))  (dw (+ #x0c00 (avr-reg-s src) (avr-reg-d dst)))]
   [`(adc  ,(? avr-reg? src) ,(? avr-reg? dst))  (dw (+ #x1c00 (avr-reg-s src) (avr-reg-d dst)))]
   [`(and  ,(? avr-reg? src) ,(? avr-reg? dst))  (dw (+ #x2000 (avr-reg-s src) (avr-reg-d dst)))]
   [`(and  ,(? avr-reg? src) ,(? avr-reg? dst))  (dw (+ #x2000 (avr-reg-s src) (avr-reg-d dst)))]
   [`(cp   ,(? avr-reg? src) ,(? avr-reg? dst))  (dw (+ #x1400 (avr-reg-s src) (avr-reg-d dst)))]
   [`(cpc  ,(? avr-reg? src) ,(? avr-reg? dst))  (dw (+ #x0400 (avr-reg-s src) (avr-reg-d dst)))]
   [`(cpse ,(? avr-reg? src) ,(? avr-reg? dst))  (dw (+ #x1000 (avr-reg-s src) (avr-reg-d dst)))]
   [`(eor  ,(? avr-reg? src) ,(? avr-reg? dst))  (dw (+ #x2400 (avr-reg-s src) (avr-reg-d dst)))]
   [`(mov  ,(? avr-reg? src) ,(? avr-reg? dst))  (dw (+ #x2c00 (avr-reg-s src) (avr-reg-d dst)))]
   [`(mul  ,(? avr-reg? src) ,(? avr-reg? dst))  (dw (+ #x9c00 (avr-reg-s src) (avr-reg-d dst)))]
   [`(or   ,(? avr-reg? src) ,(? avr-reg? dst))  (dw (+ #x2800 (avr-reg-s src) (avr-reg-d dst)))]
   [`(sbc  ,(? avr-reg? src) ,(? avr-reg? dst))  (dw (+ #x0800 (avr-reg-s src) (avr-reg-d dst)))]
   [`(sub  ,(? avr-reg? src) ,(? avr-reg? dst))  (dw (+ #x1800 (avr-reg-s src) (avr-reg-d dst)))]
   
   [`(adiw ,(? avr-reg-pair-24-30? src) ,(? avr-imm-6b? b)) (dw (+ #x9600 (avr-reg-pair-24-30 src) (avr-imm-6b b)))]
   
   [`(andi ,(? avr-reg-hi? src) ,(? avr-imm-b? b))          (dw (+ #x7000 (avr-reg-hi-d src) (avr-imm-b b)))]
   
   [`(asr ,(? avr-reg? src)) (dw (+ #x9405 (avr-reg-d src)))]
   
   [`(bclr ,(? avr-bit? bit)) (dw (+ #x9488 (avr-bit-d bit)))]
   [`(bset ,(? avr-bit? bit)) (dw (+ #x9408 (avr-bit-d bit)))]
   
   [`(bld  ,(? avr-reg? src) ,(? avr-bit? bit))  (dw (+ #xf800 bit (avr-reg-d src)))]
   
   [`(brbc ,(? avr-bit? s) ,(? avr-rel-7b? dst)) (dw (+ #xf400 s (avr-rel-7b dst)))]
   [`(brbs ,(? avr-bit? s) ,(? avr-rel-7b? dst)) (dw (+ #xf000 s (avr-rel-7b dst)))]
   
   [`(brcc ,(? avr-rel-7b? dst)) (dw (+ #xf400 (avr-rel-7b dst)))]
   [`(brsh ,(? avr-rel-7b? dst)) (dw (+ #xf400 (avr-rel-7b dst)))]
   [`(brcs ,(? avr-rel-7b? dst)) (dw (+ #xf000 (avr-rel-7b dst)))]
   [`(brlo ,(? avr-rel-7b? dst)) (dw (+ #xf000 (avr-rel-7b dst)))]
   [`(breq ,(? avr-rel-7b? dst)) (dw (+ #xf401 (avr-rel-7b dst)))]
   [`(brne ,(? avr-rel-7b? dst)) (dw (+ #xf001 (avr-rel-7b dst)))]
   [`(brge ,(? avr-rel-7b? dst)) (dw (+ #xf404 (avr-rel-7b dst)))]
   [`(brlt ,(? avr-rel-7b? dst)) (dw (+ #xf004 (avr-rel-7b dst)))]
   [`(brhc ,(? avr-rel-7b? dst)) (dw (+ #xf405 (avr-rel-7b dst)))]
   [`(brhs ,(? avr-rel-7b? dst)) (dw (+ #xf005 (avr-rel-7b dst)))]
   [`(brid ,(? avr-rel-7b? dst)) (dw (+ #xf407 (avr-rel-7b dst)))]
   [`(brie ,(? avr-rel-7b? dst)) (dw (+ #xf007 (avr-rel-7b dst)))]
   [`(brmi ,(? avr-rel-7b? dst)) (dw (+ #xf402 (avr-rel-7b dst)))]
   [`(brpl ,(? avr-rel-7b? dst)) (dw (+ #xf002 (avr-rel-7b dst)))]
   [`(brtc ,(? avr-rel-7b? dst)) (dw (+ #xf406 (avr-rel-7b dst)))]
   [`(brts ,(? avr-rel-7b? dst)) (dw (+ #xf006 (avr-rel-7b dst)))]
   [`(brvc ,(? avr-rel-7b? dst)) (dw (+ #xf403 (avr-rel-7b dst)))]
   [`(brvs ,(? avr-rel-7b? dst)) (dw (+ #xf003 (avr-rel-7b dst)))]
   
   
   ))
