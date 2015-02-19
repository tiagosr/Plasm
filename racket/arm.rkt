#lang racket

(require "plasm.rkt")

(define (arm-imm.b? n)    (between? -128 n 255))
(define (arm-imm.w? n)    (between? -32768 n 65535))
(define (arm-imm.l? n)    (between? (- #x80000000) n #xfffffffff))
(define (arm-disp.s? n)   (between? -128 n 127))
(define (arm-disp.w? n)   (between? -32768 n 32767))
(define (arm-pcrel? n)    (between? -32760 n 32768))

(define arm-regs #hash(
    [r0 . 0]
    [r1 . 1]
    [r2 . 2]
    [r3 . 3]
    [r4 . 4]
    [r5 . 5]
    [r6 . 6]
    [r7 . 7]
    [r8 . 8]
    [r9 . 9]
    [r10 . 10]
    [r11 . 11]
    [r12 . 12]
    [r13 . 13]
    [r14 . 14]
    [lr . 14]
    [r15 . 15]
    [pc . 15]))

(define (arm-reg? reg)
  (hash-has-key? arm-regs reg))

(define (arm-reg-pc? reg)
  (member reg '(r15 pc)))

(define (arm-reg-num reg)
  (hash-ref arm-regs reg))

(define (arm-reg-16-19 reg)
  (<< (arm-reg-num reg) 16))

(define (arm-reg-8-15 reg)
  (<< (arm-reg-num reg) 8))

(define (arm-reg-4-7 reg)
  (<< (arm-reg-num reg) 4))


(define arm-conds #hash(
    [eq . #x00000000]
    [ne . #x10000000]
    [cs . #x20000000]
    [hs . #x20000000]
    [cc . #x30000000]
    [lo . #x40000000]
    [mi . #x50000000]
    [pl . #x60000000]
    [vs . #x70000000]
    [vc . #x80000000]
    [hi . #x90000000]
    [ls . #xa0000000]
    [ge . #xb0000000]
    [lt . #xc0000000]
    [gt . #xd0000000]
    [al . #xe0000000]))

(define (arm-op-cond cond)
  (hash-ref arm-conds cond))

(define (arm-cond? cond)
  (hash-has-key? arm-conds cond))

(define (arm-op-strip-conds op)
  (letrec ([str (symbol->string op)]
           [spstr (string-split str ".")]
           [opstr (first spstr)])
    (if (< 1 (length spstr))
        (if (arm-cond? (second spstr))
            (string->symbol opstr)
            #f)
        op)))

(define arm-ops-dataproc
  #hash(
    [and  . #x0000000]
    [ands . #x0100000]
    [eor  . #x0200000]
    [eors . #x0300000]
    [sub  . #x0400000]
    [subs . #x0500000]
    [rsb  . #x0600000]
    [rsbs . #x0700000]
    [add  . #x0800000]
    [adds . #x0900000]
    [adc  . #x0a00000]
    [adcs . #x0b00000]
    [sbc  . #x0c00000]
    [sbcs . #x0d00000]
    [rsc  . #x0e00000]
    [rscs . #x0f00000]
    ;
    [tsts . #x1100000]
    ;
    [teqs . #x1300000]
    ;
    [cmps . #x1500000]
    ;
    [cmns . #x1700000]
    [orr  . #x1800000]
    [orrs . #x1900000]
    [mov  . #x1a00000]
    [movs . #x1b00000]
    [bic  . #x1c00000]
    [bics . #x1d00000]
    [mvn  . #x1e00000]
    [mvns . #x1f00000]
    ))

(define (arm-op-dataproc? op)
  (hash-has-key? arm-ops-dataproc (arm-op-strip-conds op)))

(define (arm-op-dataproc op)
  (hash-ref arm-ops-dataproc (arm-op-strip-conds op)))

(define (arm-op-branch? op)
  (in-list? (arm-op-strip-conds op)
            '[b bl]))

(define armv5-op-map
  (match-lambda
    [`(,(? arm-op-dataproc? op) ,(? arm-reg? dest) ,(? arm-reg? src1) ,(? arm-reg? src2))
     (dd (+ (arm-op-dataproc op) (arm-reg-16-19 dest) (arm-reg-8-15 src1) (arm-reg-4-7 src2)))]))

(make-architecture 'armv5le #f armv5-op-map)
(make-architecture 'armv5 #t armv5-op-map)