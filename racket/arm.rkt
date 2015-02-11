#lang racket

(require "plasm.rkt")

(define (arm-imm.b? n)    (between? -128 n 255))
(define (arm-imm.w? n)    (between? -32768 n 65535))
(define (arm-imm.l? n)    (between? (- #x80000000) n #xfffffffff))
(define (arm-disp.s? n)   (between? -128 n 127))
(define (arm-disp.w? n)   (between? -32768 n 32767))
(define (arm-pcrel? n)    (between? -32760 n 32768))

(define (arm-reg? reg)
  (member reg '(r0 r1 r2 r3 r4 r5 r6 r7
                r8 r9 r10 r11 r12 r13 r14 r15 lr pc sp)))


(define (arm-reg-pc? reg)
  (member reg '(r15 pc)))

(define arm-reg-num
  (match-lambda
    ['r0 0]
    ['r1 1]
    ['r2 2]
    ['r3 3]
    ['r4 4]
    ['r5 5]
    ['r6 6]
    ['r7 7]
    ['r8 8]
    ['r9 9]
    ['r10 10]
    ['r11 11]
    ['r12 12]
    ['r13 13]
    ['r14 14]
    ['lr 14]
    ['r15 15]
    ['pc 15]))

(define (arm-reg-16-19 reg)
  (<< (arm-reg-num reg) 16))

(define (arm-reg-8-15 reg)
  (<< (arm-reg-num reg) 8))

(define (arm-reg-4-7 reg)
  (<< (arm-reg-num reg) 4))

(define arm-op-cond
  (match-lambda
    ['eq #x00000000]
    ['ne #x10000000]
    ['cs #x20000000]
    ['hs #x20000000]
    ['cc #x30000000]
    ['lo #x40000000]
    ['mi #x50000000]
    ['pl #x60000000]
    ['vs #x70000000]
    ['vc #x80000000]
    ['hi #x90000000]
    ['ls #xa0000000]
    ['ge #xb0000000]
    ['lt #xc0000000]
    ['gt #xd0000000]
    ['al #xe0000000]))

(define (arm-cond? cond)
  (member ['eq 'ne 'cs 'hs 'cc 'lo 'mi 'pl
               'vs 'vc 'ls 'ge 'lt 'gt 'al]))

(define (arm-op-strip-conds op)
  (letrec ([str (symbol->string op)]
           [spstr (string-split str ".")]
           [opstr (first spstr)])
    (if (arm-cond? (second spstr))
        (string->symbol opstr)
        #f)))

(define (arm-op-dataproc? op)
  (member (arm-op-strip-conds op)
    ['adc 'add 'and 'bic 'cmn 'cmp 'eor 'mov
     'mvn 'orr 'rsb 'rsc 'sbc 'sub 'teq 'tst]))
(define (arm-op-branch? op)
  (member (arm-op-strip-conds op)
          ['b]))

(define (arm-op? op)
  (or (arm-op-dataproc? op)
      ))

;(make-architecture
; 'armv5le #f
; )
