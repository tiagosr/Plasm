#lang racket

(require "plasm.rkt")

(define (arm-imm.b? n)    (between? -128 n 255))
(define (arm-imm.w? n)    (between? -32768 n 65535))
(define (arm-imm.l? n)    (between? (- #x80000000) n #xfffffffff))
(define (arm-disp.s? n)   (between? -128 n 127))
(define (arm-disp.w? n)   (between? -32768 n 32767))
(define (arm-pcrel? n)    (between? -32760 n 32768))
(define (arm-word-aligned? n)
  (= n (& #xfffffffa n)))
(define (arm-offset-rel? n)
  (and (between? (- #x800000) (>> n 2) #x7fffff)
       (arm-word-aligned? n)))

(define arm-regs #hash(
    [r0  . 0]
    [r1  . 1]
    [r2  . 2]
    [r3  . 3]
    [r4  . 4]
    [r5  . 5]
    [r6  . 6]
    [r7  . 7]
    [r8  . 8]
    [r9  . 9]
    [r10 . 10]
    [r11 . 11]
    [r12 . 12]
    [r13 . 13]
    [sr  . 13]
    [r14 . 14]
    [lr  . 14]
    [r15 . 15]
    [pc  . 15]))

(define (arm-reg? reg)
  (hash-has-key? arm-regs reg))

(define (arm-reg-pc? reg)
  (member reg '(r15 pc)))

(define (arm-reg-num reg)
  (hash-ref arm-regs reg))

(define (arm-reg-16-19 reg)
  (<< (arm-reg-num reg) 16))

(define (arm-reg-12-15 reg)
  (<< (arm-reg-num reg) 12))

(define (arm-reg-4-7 reg)
  (<< (arm-reg-num reg) 4))

(define (arm-op2-rotate-imm? val)
  (for/or ([x (in-range 0 30 2)])
    (let ([x-rev-rot (><<-d val x)])
      (= (|| x-rev-rot 255) 255))))
(define (arm-op2-rotate-count val)
  (/ (for/or ([x (in-range 0 30 2)])
       (let ([x-rev-rot (><<-d val x)])
         (if (= (|| x-rev-rot 255) 255)
             x
             #f)) 2)))
(define (arm-op2-rotate-val val)
  (for/or ([x (in-range 0 30 2)])
     (let ([x-rev-rot (><<-d val x)])
       (if (= (|| x-rev-rot 255) 255)
           x-rev-rot
           #f))))

(define (arm-op2-immediate val)
  (|| #x04000000 (<< (arm-op2-rotate-count val) 8) (arm-op2-rotate-val val)))


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
           [opstr (string-join (take spstr (- (length spstr) 1)) ".")])
    (if (> (length spstr) 1)
        (if (arm-cond? (string->symbol (last spstr)))
            (string->symbol opstr)
            op)
        op)))
(define (arm-cond-val op)
  (letrec ([str (symbol->string op)]
           [spstr (string-split str ".")]
           [opstr (first spstr)])
    (if (> (length spstr) 1)
        (if (arm-cond? (last spstr))
            (hash-ref arm-conds (string->symbol (last spstr)))
            (hash-ref arm-conds 'al))
        (hash-ref arm-conds 'al))))

(define arm-shifter-ops
  #hash(
        [lsl . 0]
        
        ))
        

(define arm-ops-dataproc
  #hash(
    [and   . #x0000000]
    [and.s . #x0100000]
    [eor   . #x0200000]
    [eor.s . #x0300000]
    [sub   . #x0400000]
    [sub.s . #x0500000]
    [rsb   . #x0600000]
    [rsb.s . #x0700000]
    [add   . #x0800000]
    [add.s . #x0900000]
    [adc   . #x0a00000]
    [adc.s . #x0b00000]
    [sbc   . #x0c00000]
    [sbc.s . #x0d00000]
    [rsc   . #x0e00000]
    [rsc.s . #x0f00000]
    ;
    [tst   . #x1100000]
    [tst.s . #x1100000]
    ;
    [teq   . #x1300000]
    [teq.s . #x1300000]
    ;
    [cmp   . #x1500000]
    [cmp.s . #x1500000]
    ;
    [cmn   . #x1700000]
    [cmn.s . #x1700000]
    [orr   . #x1800000]
    [orr.s . #x1900000]
    [mov   . #x1a00000]
    [mov.s . #x1b00000]
    [bic   . #x1c00000]
    [bic.s . #x1d00000]
    [mvn   . #x1e00000]
    [mvn.s . #x1f00000]
    ))

(define (arm-op-dataproc? op)
  (hash-has-key? arm-ops-dataproc (arm-op-strip-conds op)))

(define (arm-op-dataproc op)
  (hash-ref arm-ops-dataproc (arm-op-strip-conds op)))

(define arm-ops-branch
  #hash(
        [b  . #x0a000000]
        [bl . #x0b000000]))

(define (arm-op-branch? op)
  (hash-has-key? arm-ops-branch (arm-op-strip-conds op)))
(define (arm-op-branch op offset)
  (+ (hash-ref arm-ops-branch (arm-op-strip-conds op)) (arm-cond-val op) (& (>> offset 2) #xffffff)))



(define armv5-op-map
  (match-lambda
    [`(,(? arm-op-dataproc? op) ,(? arm-reg? dest) ,(? arm-reg? src1) ,(? arm-reg? src2))
     (dd (+ (arm-cond-val op) (arm-op-dataproc op) (arm-reg-16-19 src1) (arm-reg-12-15 dest) (arm-reg-4-7 src2)))]
    [`(,(? arm-op-branch? op) ,(? arm-offset-rel? offset))
     (dd (arm-op-branch op offset))]
    ))

(make-architecture 'armv5le #f armv5-op-map)
(make-architecture 'armv5 #t armv5-op-map)