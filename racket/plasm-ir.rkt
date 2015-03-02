#lang racket

(define ir-node%
  (class object%
    (init ppt)
    (define program-point ppt)
    (super-new)
    (define/public (valid?) #f)
    (define/public (symbolic?) #t)
    (define/public (get-value)
      null)
    (define/public (to-string)
      "<invalid node>")
    (define/public (accept pass env)
      (send pass visit this env))
    ))

; constant value, usually needs no treatment
(define constant%
  (class ir-node%
    (init ppt constant-value)
    (define value constant-value)
    (super-instantiate [ppt])
    (define/override (valid?) #t)
    (define/override (symbolic?) #f)
    (define/override (get-value) value)
    (define/override (to-string) (~a value))
    ))

; label value, gets linked at the later stages of compilation
(define label%
  (class ir-node%
    (init ppt label-name label-offset)
    (define name label-name)
    (define offset label-offset)
    (super-instantiate [ppt])
    (define/override (symbolic?) #t)
    ))

; pc-relative value, gets an offset modified by the current pc
(define pc-rel%
  (class ir-node%
    (init ppt rel-node)
    (super-instantiate [ppt])
    (define node rel-node)
    (define/override (symbolic?) (send node symbolic?))
    ))

; value piece
(define piece%
  (class ir-node%
    (init ppt)
    (super-instantiate [ppt])
    (init-field value
                in-shift
                out-shift
                bits)
    (define/override (symbolic?) (or (send value symbolic?)
                                     (send in-shift symbolic?)
                                     (send out-shift symbolic?)
                                     (send bits symbolic?)))
    (define/override (valid?) (and (send value valid?)
                                   (send in-shift valid?)
                                   (send out-shift valid?)
                                   (send bits valid?)))
    (define/override (get-value)
      (arithmetic-shift (bitwise-and (- (expt 2 (send bits get-value)) 1) 
                                     (arithmetic-shift (send value get-value)
                                                       (send in-shift get-value)))
          (send out-shift get-value)))
    ))
   
; operation
(define operation%
  (class ir-node%
    (init ppt op-lhs operation op-rhs)
    (define op operation)
    (define lhs op-lhs)
    (define rhs op-rhs)
    (super-new [ppt ppt])
    (define/override (symbolic?)
      (or (send lhs symbolic?)
          (send rhs symbolic?)))
    (define/override (valid?)
      (and (send lhs valid?)
           (send rhs valid?)))
    ))

