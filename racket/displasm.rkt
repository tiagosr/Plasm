#lang racket

(define (make-label-for-loc loc)
  (string->symbol (format "loc_~a:" loc)))

(define (make-label-ref-for-loc loc)
  (string->symbol (format "loc_~a" loc)))

