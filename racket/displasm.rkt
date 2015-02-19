#lang racket

(define base-address 0)

(define (make-label-for-loc loc)
  (string->symbol (format "loc_~x:" loc)))

(define (make-label-ref-for-loc loc)
  (string->symbol (format "loc_~x" loc)))

(define (set-base-address addr)
  (set! base-address addr))

