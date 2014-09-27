#lang racket

(require "plasm.rkt")

(define (avr-reg-lo? r)
  (member '(r0 r1 r2 r3 r4 r5 r6 r7
            r8 r9 r10 r11 r12 r13 r14 r15) r))
(define (avr-reg-hi? r)
  (member '(r16 r17 r18 r19 r20 r21 r22 r23
            r24 r25 r26 r27 r28 r29 r30 r31) r))
(define (avr-reg? r)
  (or (avr-reg-lo? r) (avr-reg-hi? r)))
            
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
   
   
   ))
