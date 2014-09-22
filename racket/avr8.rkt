#lang racket

(require "plasm.rkt")

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
