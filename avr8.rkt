#lang racket

(require "plasm.rkt")

(define (avr-reg-lo? r)
  (in-list? r '(r0 r1 r2 r3 r4 r5 r6 r7
                r8 r9 r10 r11 r12 r13 r14 r15)))
(define (avr-reg-hi? r)
  (in-list? r '(r16 r17 r18 r19 r20 r21 r22 r23
                r24 r25 r26 r27 r28 r29 r30 r31)))
(define (avr-reg-pair? r)
  (in-list? r '(z y x
                r31:r30 r29:r28 r27:r26 r25:r24
                r23:r22 r21:r20 r19:r18 r17:r16
                r15:r14 r13:r12 r11:r10 r9:r8
                r7:r6 r5:r4 r3:r2 r1:r0)))
(define (avr-reg-pair-24-30? r)
  (in-list? r '(r25:r24 r27:r26 r29:r28 r31:r30 x y z)))

(define (avr-reg? r)
  (or (avr-reg-lo? r) (avr-reg-hi? r)))

(define avr-reg-s
  (match-lambda
    ['r0 #x0]    ['r1 #x1]    ['r2 #x2]    ['r3 #x3]
    ['r4 #x4]    ['r5 #x5]    ['r6 #x6]    ['r7 #x7]
    ['r8 #x8]    ['r9 #x9]    ['r10 #xa]   ['r11 #xb]
    ['r12 #xc]   ['r13 #xd]   ['r14 #xe]   ['r15 #xf]
    ['r16 #x200] ['r17 #x201] ['r18 #x202] ['r19 #x203]
    ['r20 #x204] ['r21 #x205] ['r22 #x206] ['r23 #x207]
    ['r24 #x208] ['r25 #x209] ['r26 #x20a] ['r27 #x20b]
    ['r28 #x20c] ['r29 #x20d] ['r30 #x20e] ['r31 #x20f]))

(define avr-reg-d
  (match-lambda
    ['r0 #x00]   ['r1 #x10]   ['r2 #x20]   ['r3 #x30]
    ['r4 #x40]   ['r5 #x50]   ['r6 #x60]   ['r7 #x70]
    ['r8 #x80]   ['r9 #x90]   ['r10 #xa0]  ['r11 #xb0]
    ['r12 #xc0]  ['r13 #xd0]  ['r14 #xe0]  ['r15 #xf0]
    ['r16 #x100] ['r17 #x110] ['r18 #x120] ['r19 #x130]
    ['r20 #x140] ['r21 #x150] ['r22 #x160] ['r23 #x170]
    ['r24 #x180] ['r25 #x190] ['r26 #x1a0] ['r27 #x1b0]
    ['r28 #x1c0] ['r29 #x1d0] ['r30 #x1e0] ['r31 #x1f0]))

(define avr-reg-hi-d
  (match-lambda
    ['r16 #x00] ['r17 #x10] ['r18 #x20] ['r19 #x30]
    ['r20 #x40] ['r21 #x50] ['r22 #x60] ['r23 #x70]
    ['r24 #x80] ['r25 #x90] ['r26 #xa0] ['r27 #xb0]
    ['r28 #xc0] ['r29 #xd0] ['r30 #xe0] ['r31 #xf0]))
(define avr-reg-hi-r
  (match-lambda
    ['r16 #x0] ['r17 #x1] ['r18 #x2] ['r19 #x3]
    ['r20 #x4] ['r21 #x5] ['r22 #x6] ['r23 #x7]
    ['r24 #x8] ['r25 #x9] ['r26 #xa] ['r27 #xb]
    ['r28 #xc] ['r29 #xd] ['r30 #xe] ['r31 #xf]))

(define avr-reg-pair-d
  (match-lambda
    ['r31:r30 #xf0] ['z #xf0]
    ['r29:r28 #xe0] ['y #xe0]
    ['r27:r26 #xd0] ['x #xd0]
    ['r25:r24 #xc0]
    ['r23:r22 #xb0] ['r21:r20 #xa0] ['r19:r18 #x90] ['r17:r16 #x80]
    ['r15:r14 #x70] ['r13:r12 #x60] ['r11:r10 #x50] ['r9:r8 #x40]
    ['r7:r6 #x30]   ['r5:r4 #x20]   ['r3:r2 #x10]   ['r1:r0 0]))
(define avr-reg-pair-r
  (match-lambda
    ['r31:r30 15] ['z 15]
    ['r29:r28 14] ['y 14]
    ['r27:r26 13] ['x 13]
    ['r25:r24 12]
    ['r23:r22 11] ['r21:r20 10] ['r19:r18 9]  ['r17:r16 8]
    ['r15:r14 7]  ['r13:r12 6]  ['r11:r10 5]  ['r9:r8 4]
    ['r7:r6 3]    ['r5:r4 2]    ['r3:r2 1]    ['r1:r0 0]))
(define avr-reg-pair-24-30
  (match-lambda
    ['r25:r24 0]
    ['r27:r26 1] ['x 1]
    ['r29:r28 2] ['y 2]
    ['r31:r30 3] ['z 3]))

(define (avr-reg-fmul? r)
  (in-list? r '(r16 r17 r18 r19 r20 r21 r22 r23)))
(define avr-reg-fmul-r
  (match-lambda
    ['r16 0] ['r17 1] ['r18 2] ['r19 3]
    ['r20 4] ['r21 5] ['r22 6] ['r23 7]))
(define avr-reg-fmul-d
  (match-lambda
    ['r16 #x00] ['r17 #x10] ['r18 #x20] ['r19 #x30]
    ['r20 #x40] ['r21 #x50] ['r22 #x60] ['r23 #x70]))

(define (avr-imm-6b? i) (between? 0 i 63))
(define (avr-imm-6b i) (+ (& #x0f i) (<< (& #x30 i) 2)))
(define (avr-imm-6q i) (+ (<< (bit-field i 5 6) 13)  (<< (bit-field i 3 5) 10 ) (bit-field i 0 3) ))

(define (avr-imm-b? i) (between? -128 i 127))
(define (avr-imm-b i) (+ (& #x0f i) (<< (& #xf0 i) 4)))

(define (avr-imm-16b? i) (between? 0 i 65535))
(define (avr-imm-16b i) i)

(define (avr-bit? i) (between? 0 i 7))
(define (avr-bit-d i) (<< i 4))

; relative address is word-aligned
(define (avr-rel-7b? i) (between? -64 (>> (+ 2 (->@ i)) 1) 63))
(define (avr-rel-7b i) (<< (& #xfe (+ 2 (->@ i))) 2))
(define (avr-rel-12b? i) (between? -2048 (>> (+ 2 (->@ i)) 1) 2046))
(define (avr-rel-12b i) (bit-field (->@ i) 1 12))

; I/O locations
(define (avr-io? io) (between? 0 io 63))
(define (avr-io io) (+ (& #xf io) (<< (& #x30 io) 5)))
(define (avr-io-lo? io) (between? 0 io 31))
(define (avr-io-lo io) (<< io 3))

; SRAM locations
(define (avr-sram? addr) (and (between? 0 addr 65535) (= 0 (& addr 1))))
(define (avr-sram addr) (>> addr 1))

; Code ROM locations
(define (avr-crom? addr) (and (between? 0 addr 8388607) (= 0 (& addr 1))))
(define (avr-crom-lo addr) (bit-field addr 1 16))
(define (avr-crom-hi addr) (or (bit-field addr 16 17) (<< (bit-field addr 17 22) 4)))


(make-architecture
 'avr8 #t
 (match-lambda
   [`(nop)      (dw      0)]
   [`(ret)      (dw #x9508)]
   [`(reti)     (dw #x9518)]
   [`(icall)    (dw #x9509)]
   [`(ijmp)     (dw #x9409)]
   [`(eicall)   (dw #x9519)]
   [`(eijmp)    (dw #x9419)]
   [`(sleep)    (dw #x9588)]
   [`(break)    (dw #x9598)]
   [`(wdr)      (dw #x95a8)]
   
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

   [`(lsl ,(? avr-reg? reg)) (dw (+ #x0c00 (avr-reg-s reg) (avr-reg-d reg)))] ; same as (add reg reg)
   [`(rol ,(? avr-reg? reg)) (dw (+ #x1c00 (avr-reg-s reg) (avr-reg-d reg)))] ; same as (adc reg reg)
   [`(tst ,(? avr-reg? reg)) (dw (+ #x2000 (avr-reg-s reg) (avr-reg-d reg)))] ; same as (and reg reg)
   
   [`(adiw ,(? avr-reg-pair-24-30? src) ,(? avr-imm-6b? b)) (dw (+ #x9600 (avr-reg-pair-24-30 src) (avr-imm-6b b)))]
   [`(sbiw ,(? avr-reg-pair-24-30? src) ,(? avr-imm-6b? b)) (dw (+ #x9700 (avr-reg-pair-24-30 src) (avr-imm-6b b)))]

   [`(movw ,(? avr-reg-pair? d) ,(? avr-reg-pair? r))       (dw (+ #x0100 (avr-reg-pair-d d) (avr-reg-pair-r r)))]
   
   [`(cpi ,(? avr-reg-hi? src) ,(? avr-imm-b? b))           (dw (+ #x3000 (avr-reg-hi-d src) (avr-imm-b b)))]
   [`(subi ,(? avr-reg-hi? src) ,(? avr-imm-b? b))          (dw (+ #x5000 (avr-reg-hi-d src) (avr-imm-b b)))]
   [`(ori ,(? avr-reg-hi? src) ,(? avr-imm-b? b))           (dw (+ #x6000 (avr-reg-hi-d src) (avr-imm-b b)))]
   [`(andi ,(? avr-reg-hi? src) ,(? avr-imm-b? b))          (dw (+ #x7000 (avr-reg-hi-d src) (avr-imm-b b)))]
   
   [`(sbci ,(? avr-reg-hi? src) ,(? avr-imm-b? b))          (dw (+ #x4000 (avr-reg-hi-d src) (avr-imm-b b)))]
   
   [`(swap ,(? avr-reg? reg)) (dw (+ #x9402 (avr-reg-d reg)))]
   [`(asr ,(? avr-reg? reg))  (dw (+ #x9405 (avr-reg-d reg)))]
   [`(lsr ,(? avr-reg? reg))  (dw (+ #x9406 (avr-reg-d reg)))]
   [`(ror ,(? avr-reg? reg))  (dw (+ #x9407 (avr-reg-d reg)))]
   
   [`(xch z ,(? avr-reg? reg)) (dw (+ #x9204 (avr-reg-d reg)))]
   [`(las z ,(? avr-reg? reg)) (dw (+ #x9205 (avr-reg-d reg)))]
   [`(lac z ,(? avr-reg? reg)) (dw (+ #x9206 (avr-reg-d reg)))]
   [`(lat z ,(? avr-reg? reg)) (dw (+ #x9207 (avr-reg-d reg)))]
   
   [`(pop ,(? avr-reg? reg))  (dw (+ #x900f (avr-reg-d reg)))]
   [`(push ,(? avr-reg? reg)) (dw (+ #x920f (avr-reg-d reg)))]
   
   [`(bclr ,(? avr-bit? bit)) (dw (+ #x9488 (avr-bit-d bit)))]
   [`(bset ,(? avr-bit? bit)) (dw (+ #x9408 (avr-bit-d bit)))]
   
   [`(bld  ,(? avr-reg? dst) ,(? avr-bit? bit))  (dw (+ #xf800 bit (avr-reg-d dst)))]
   [`(bst  ,(? avr-reg? src) ,(? avr-bit? bit))  (dw (+ #xfa00 bit (avr-reg-d src)))]
   [`(sbrc ,(? avr-reg? reg) ,(? avr-bit? bit))  (dw (+ #xfc00 bit (avr-reg-d reg)))]
   [`(sbrs ,(? avr-reg? reg) ,(? avr-bit? bit))  (dw (+ #xfe00 bit (avr-reg-d reg)))]
   
   [`(sbr ,(? avr-reg-hi? src) ,(? avr-imm-b? b))  (dw (+ #x6000 (avr-reg-hi-d src) (avr-imm-b b)))]
   
   [`(ser ,(? avr-reg-hi? src))  (dw (+ #xef0f (avr-reg-hi-d src)))]
   
   [`(brbc ,(? avr-bit? s) ,(? avr-rel-7b? dst)) (dw (+ #xf400 s (avr-rel-7b dst)))]
   [`(brbs ,(? avr-bit? s) ,(? avr-rel-7b? dst)) (dw (+ #xf000 s (avr-rel-7b dst)))]
   
   [`(brcc ,(? avr-rel-7b? dst)) (dw (+ #xf400 (avr-rel-7b dst)))]
   [`(brsh ,(? avr-rel-7b? dst)) (dw (+ #xf400 (avr-rel-7b dst)))]
   [`(brcs ,(? avr-rel-7b? dst)) (dw (+ #xf000 (avr-rel-7b dst)))]
   [`(brlo ,(? avr-rel-7b? dst)) (dw (+ #xf000 (avr-rel-7b dst)))]
   [`(breq ,(? avr-rel-7b? dst)) (dw (+ #xf001 (avr-rel-7b dst)))]
   [`(brne ,(? avr-rel-7b? dst)) (dw (+ #xf401 (avr-rel-7b dst)))]
   [`(brge ,(? avr-rel-7b? dst)) (dw (+ #xf404 (avr-rel-7b dst)))]
   [`(brlt ,(? avr-rel-7b? dst)) (dw (+ #xf004 (avr-rel-7b dst)))]
   [`(brhc ,(? avr-rel-7b? dst)) (dw (+ #xf405 (avr-rel-7b dst)))]
   [`(brhs ,(? avr-rel-7b? dst)) (dw (+ #xf005 (avr-rel-7b dst)))]
   [`(brid ,(? avr-rel-7b? dst)) (dw (+ #xf407 (avr-rel-7b dst)))]
   [`(brie ,(? avr-rel-7b? dst)) (dw (+ #xf007 (avr-rel-7b dst)))]
   [`(brmi ,(? avr-rel-7b? dst)) (dw (+ #xf002 (avr-rel-7b dst)))]
   [`(brpl ,(? avr-rel-7b? dst)) (dw (+ #xf402 (avr-rel-7b dst)))]
   [`(brtc ,(? avr-rel-7b? dst)) (dw (+ #xf406 (avr-rel-7b dst)))]
   [`(brts ,(? avr-rel-7b? dst)) (dw (+ #xf006 (avr-rel-7b dst)))]
   [`(brvc ,(? avr-rel-7b? dst)) (dw (+ #xf403 (avr-rel-7b dst)))]
   [`(brvs ,(? avr-rel-7b? dst)) (dw (+ #xf003 (avr-rel-7b dst)))]

   [`(com ,(? avr-reg? reg))  (dw (+ #x9400 (avr-reg-d reg)))]
   [`(neg ,(? avr-reg? reg))  (dw (+ #x9401 (avr-reg-d reg)))]
   [`(swap ,(? avr-reg? reg)) (dw (+ #x9402 (avr-reg-d reg)))]
   [`(inc ,(? avr-reg? reg))  (dw (+ #x9403 (avr-reg-d reg)))]
   [`(dec ,(? avr-reg? reg))  (dw (+ #x940a (avr-reg-d reg)))]

   [`(muls ,(? avr-reg-hi? d) ,(? avr-reg-hi? r))       (dw (+ #x0200 (avr-reg-hi-d d) (avr-reg-hi-r r)))]

   [`(mulsu ,(? avr-reg-fmul? d) ,(? avr-reg-fmul? r))  (dw (+ #x0300 (avr-reg-fmul-r r) (avr-reg-fmul-d d)))]
   [`(fmul ,(? avr-reg-fmul? d) ,(? avr-reg-fmul? r))   (dw (+ #x0308 (avr-reg-fmul-r r) (avr-reg-fmul-d d)))]
   [`(fmuls ,(? avr-reg-fmul? d) ,(? avr-reg-fmul? r))  (dw (+ #x0380 (avr-reg-fmul-r r) (avr-reg-fmul-d d)))]
   [`(fmulsu ,(? avr-reg-fmul? d) ,(? avr-reg-fmul? r)) (dw (+ #x0388 (avr-reg-fmul-r r) (avr-reg-fmul-d d)))]

   ; load/store program memory
   [`(lpm)                      (dw #x95c8)]
   [`(lpm ,(? avr-reg? reg) z)  (dw (+ #x9004 (avr-reg-d reg)))]
   [`(lpm ,(? avr-reg? reg) z+) (dw (+ #x9005 (avr-reg-d reg)))]
   [`(spm)                      (dw #x95e8)]
   [`(spm ,(? avr-reg? reg) z+) (dw (+ #x92f8 (avr-reg-d reg)))]
   
   [`(ldi ,(? avr-reg-hi? src) ,(? avr-imm-b? b)) (dw (+ #xe000 (avr-reg-hi-d src) (avr-imm-b b)))]
   
   [`(ld ,(? avr-reg? reg) x)  (dw (+ #x900c (avr-reg-d reg)))]
   [`(ld ,(? avr-reg? reg) x+) (dw (+ #x900d (avr-reg-d reg)))]
   [`(ld ,(? avr-reg? reg) x-)  (dw (+ #x900e (avr-reg-d reg)))]
  
   [`(st ,(? avr-reg? reg) x)  (dw (+ #x920c (avr-reg-d reg)))]
   [`(st ,(? avr-reg? reg) x+) (dw (+ #x920d (avr-reg-d reg)))]
   [`(st ,(? avr-reg? reg) x-) (dw (+ #x920e (avr-reg-d reg)))]
  
   
   [`(ld ,(? avr-reg? reg) y)  (dw (+ #x8008 (avr-reg-d reg)))]
   [`(ld ,(? avr-reg? reg) y+) (dw (+ #x9009 (avr-reg-d reg)))]
   [`(ld ,(? avr-reg? reg) y-) (dw (+ #x900a (avr-reg-d reg)))]
   [`(ldd ,(? avr-reg? reg) y+ ,(? avr-imm-6b? q)) (dw (+ #x8008 (avr-reg-d reg) (avr-imm-6q q)))]

   [`(st ,(? avr-reg? reg) y)  (dw (+ #x8208 (avr-reg-d reg)))]
   [`(st ,(? avr-reg? reg) y+) (dw (+ #x9209 (avr-reg-d reg)))]
   [`(st ,(? avr-reg? reg) y-) (dw (+ #x920a (avr-reg-d reg)))]
   [`(std ,(? avr-reg? reg) y+ ,(? avr-imm-6b? q)) (dw (+ #x8208 (avr-reg-d reg) (avr-imm-6q q)))]
   
   [`(ld ,(? avr-reg? reg) z)  (dw (+ #x8000 (avr-reg-d reg)))]
   [`(ld ,(? avr-reg? reg) z+) (dw (+ #x9001 (avr-reg-d reg)))]
   [`(ld ,(? avr-reg? reg) z-) (dw (+ #x9002 (avr-reg-d reg)))]
   [`(ldd ,(? avr-reg? reg) z+ ,(? avr-imm-6b? q))  (dw (+ #x8000 (avr-reg-d reg) (avr-imm-6q q)))]

   [`(st ,(? avr-reg? reg) z)  (dw (+ #x8200 (avr-reg-d reg)))]
   [`(st ,(? avr-reg? reg) z+) (dw (+ #x9201 (avr-reg-d reg)))]
   [`(st ,(? avr-reg? reg) z-) (dw (+ #x9202 (avr-reg-d reg)))]
   [`(std ,(? avr-reg? reg) z+ ,(? avr-imm-6b? q)) (dw (+ #x8200 (avr-reg-d reg) (avr-imm-6q q)))]


   [`(lds ,(? avr-reg? reg) ,(? avr-imm-16b? addr))  (dw (+ #x9000 (avr-reg-d reg)) (avr-imm-16b addr))]
   [`(sts ,(? avr-reg? reg) ,(? avr-imm-16b? addr)) (dw (+ #x9200 (avr-reg-d reg)) (avr-imm-16b addr))]

   
   ; extended load program memory
   [`(elpm)                      (dw #x95d8)]
   [`(elpm ,(? avr-reg? reg) z)  (dw (+ #x9006 (avr-reg-d reg)))]
   [`(elpm ,(? avr-reg? reg) z+) (dw (+ #x9007 (avr-reg-d reg)))]

   ; I/O
   [`(in ,(? avr-reg? reg) ,(? avr-io? io))      (dw (+ #xb000 (avr-reg-d reg) (avr-io io)))]
   [`(out ,(? avr-io? io) ,(? avr-reg? reg))     (dw (+ #xb800 (avr-reg-d reg) (avr-io io)))]
   [`(cbi ,(? avr-io-lo? io) ,(? avr-bit? bit))  (dw (+ #x9800 (avr-io-lo io) bit))]
   [`(sbi ,(? avr-io-lo? io) ,(? avr-bit? bit))  (dw (+ #x9a00 (avr-io-lo io) bit))]
   [`(sbic ,(? avr-io-lo? io) ,(? avr-bit? bit)) (dw (+ #x9900 (avr-io-lo io) bit))]
   [`(sbis ,(? avr-io-lo? io) ,(? avr-bit? bit)) (dw (+ #x9b00 (avr-io-lo io) bit))]

   ; absolute jumps/calls
   [`(call ,(? avr-crom? addr)) (dw (+ #x940e (avr-crom-hi addr)) (avr-crom-lo addr))]
   [`(jmp  ,(? avr-crom? addr)) (dw (+ #x940c (avr-crom-hi addr)) (avr-crom-lo addr))]

   ; relative jumps/calls
   [`(rcall ,(? avr-rel-12b? addr)) (dw (+ #xd000 (avr-rel-12b addr)))]
   [`(rjmp  ,(? avr-rel-12b? addr)) (dw (+ #xc000 (avr-rel-12b addr)))]
   
   [rest (%asm-base rest)]
   ))
