#lang racket

(struct number-promise%
  (depends
   calculate)
  #:transparent)

(define %consistency-check-vars (list))
(define (%add-consistency-check var)
  (set! %consistency-check-vars (append %consistency-check-vars (list var))))

; binary operators
(define-values (+a -a *a /a %or %and %xor %nand)
  (letrec
      ((mkop (lambda (op)
               (letrec ((p-op
                         (match-lambda* ; foldl applies arguments backwards
                           [(list (? number? a)
                                  (? number? b)) (op b a)]
                           [(list (? number-promise%? l)
                                  (? number? a)) (number-promise% (number-promise%-depends l)
                                                                  (lambda () (op a
                                                                                 ((number-promise%-calculate l)))))]
                           [(list (? number? a)
                                  (? number-promise%? l)) (number-promise% (number-promise%-depends l)
                                                                           (lambda () (op ((number-promise%-calculate l))
                                                                                          a)))]
                           [(list (? number-promise%? a)
                                  (? number-promise%? b))
                            (number-promise% (append (number-promise%-depends a)
                                                     (number-promise%-depends b))
                                             (lambda () (op ((number-promise%-calculate b))
                                                            ((number-promise%-calculate a)))))])))
                 p-op)))
       (+% (mkop +))
       (-% (mkop -))
       (*% (mkop *))
       (/% (mkop /))
       (||% (mkop bitwise-ior))
       (&% (mkop bitwise-and))
       (^% (mkop bitwise-xor))
       (~&% (mkop (lambda (a b) (bitwise-not (bitwise-and a b))))))
    (values (lambda args (foldl +% 0 args))
            (match-lambda* 
              [(list a) (-% 0 a)]
              [(list a rest ...) (foldl -% a rest)])
            (lambda args (foldl *% 1 args))
            (lambda args (foldl /% (car args) (cdr args)))
            (lambda args (foldl ||% (car args) (cdr args)))
            (lambda args (foldl &% (car args) (cdr args)))
            (lambda args (foldl ^% (car args) (cdr args)))
            (lambda args (foldl ~&% (car args) (cdr args)))
            )))

; unary operators
(define-values (%not %abs)
  (let ([mkop (lambda (op)
                (match-lambda
                  [(? number? i) (op i)]
                  [(? number-promise%? l) (number-promise% (number-promise%-depends l)
                                                           (lambda () (op ((number-promise%-calculate l)))))]
                  ))])
    (values (mkop bitwise-not)
            (mkop abs))))

; binary operators (including rotation operators of different sizes)
(define-values (<< >>
                   mod
                   ><<-n ><<-b ><<-w ><<-d ><<-q
                   >><-n >><-b >><-w >><-d >><-q)
  (letrec ([mkop (lambda (op)
                   (match-lambda*
                     [(list (? integer? a) (? integer? b)) (op a b)]
                     [(list (? number-promise%? a) (? integer? b))
                      (number-promise% (number-promise%-depends a)
                                       (lambda () (op ((number-promise%-calculate a)) b)))]
                     [(list (? integer? a) (? number-promise%? b))
                      (number-promise% (number-promise%-depends b)
                                       (lambda () (op a ((number-promise%-calculate b)))))]
                     [(list (? number-promise%? a) (? number-promise%? b))
                      (number-promise% (append (number-promise%-depends a)
                                               (number-promise%-depends b))
                                       (lambda () (op ((number-promise%-calculate a))
                                                      ((number-promise%-calculate b)))))]))]
           [%<< (mkop arithmetic-shift)]
           [%>> (mkop (lambda (a b) (arithmetic-shift a -b)))]
           [mod (mkop modulo)]
           [mkrot (lambda (size mask op)
                    (lambda (a b)
                      (let ([masked-a (%and mask a)]
                            [r (mod b size)])
                        (%and mask (%or (op masked-a r) (op masked-a (-a r size)))))))])
    (values %<< %>> mod
            (mkop (mkrot 4 #xf %<<))
            (mkop (mkrot 8 #xff %<<))
            (mkop (mkrot 16 #xffff %<<))
            (mkop (mkrot 32 #xffffffff %<<))
            (mkop (mkrot 64 #xffffffffffffffff %<<))
            (mkop (mkrot 4 #xf %>>))
            (mkop (mkrot 8 #xff %>>))
            (mkop (mkrot 16 #xffff %>>))
            (mkop (mkrot 32 #xffffffff %>>))
            (mkop (mkrot 64 #xffffffffffffffff %>>))
            )))

; comparison operators
(define-values (%< %> %= %<= %>= !=)
  (let ((mkop (lambda (op)
                (match-lambda*
                  [(list (? number? a) (? number? b)) (op a b)]
                  [(list (? number-promise%? a) (? number? b))
                   (begin
                     (%add-consistency-check (number-promise% (number-promise%-depends a)
                                                              (lambda () (op ((number-promise%-calculate a)) b))))
                     #t)]
                  [(list (? number? a) (? number-promise%? b))
                   (begin
                     (%add-consistency-check (number-promise% (number-promise%-depends b)
                                                              (lambda () (op a ((number-promise%-calculate b))))))
                     #t)]
                  [(list (? number-promise%? a) (? number-promise%? b))
                   (begin
                     (%add-consistency-check (number-promise% (append (number-promise%-depends a)
                                                                      (number-promise%-depends b))
                                                              (lambda () (op ((number-promise%-calculate a))
                                                                             ((number-promise%-calculate b))))))
                     #t)]
                  ))))
    (values (mkop <)
            (mkop >)
            (mkop =)
            (mkop <=)
            (mkop >=)
            (mkop (lambda (a b) (not (= a b)))))))

(define between?
  (match-lambda*
    [(list (? number? a)
           (? number? b)
           (? number? c)) (and (<= a b) (<= b c))]
    ; delay decision to consistency check in the other cases
    [(list (? number-promise%? a)
           (? number? b)
           (? number? c))  (if (>= b c)
                               (let ((promise (number-promise% (number-promise%-depends a)
                                                               (lambda () (<= a ((number-promise%-calculate a)))))))
                                 (%add-consistency-check (lambda () ((number-promise%-calculate promise)))) 
                                 #t)
                               #f)]
    [(list (? number? a)
           (? number? b)
           (? number-promise%? c))  (if (<= a b)
                                        (let ((promise (number-promise% (number-promise%-depends c)
                                                                        (lambda () (>= b ((number-promise%-calculate c)))))))
                                          (%add-consistency-check (lambda () ((number-promise%-calculate promise)))) 
                                          #t)
                                        #f)]
    [(list (? number? a)
           (? number-promise%? b)
           (? number? c))  (let ((promise (number-promise% (number-promise%-depends b)
                                                           (lambda () (between? a ((number-promise%-calculate b)) c)))))
                             (%add-consistency-check (lambda () ((number-promise%-calculate promise)))) 
                             #t)]
    [(list (? number-promise%? a)
           (? number-promise%? b)
           (? number? c))  (let ((promise (number-promise% (append (number-promise%-depends a)
                                                                   (number-promise%-depends b))
                                                           (lambda () (between? ((number-promise%-calculate a))
                                                                                ((number-promise%-calculate b))
                                                                                c)))))
                             (%add-consistency-check (lambda () ((number-promise%-calculate promise)))) 
                             #t)]
    [(list (? number? a)
           (? number-promise%? b)
           (? number-promise%? c))  (let ((promise (number-promise% (append (number-promise%-depends b)
                                                                            (number-promise%-depends c))
                                                                    (lambda () (between? a
                                                                                         ((number-promise%-calculate b))
                                                                                         ((number-promise%-calculate c)))))))
                                      (%add-consistency-check (lambda () ((number-promise%-calculate promise)))) 
                                      #t)]
    [(list (? number-promise%? a)
           (? number? b)
           (? number-promise%? c))  (let ((promise (number-promise% (append (number-promise%-depends a)
                                                                            (number-promise%-depends c))
                                                                    (lambda () (between? ((number-promise%-calculate a))
                                                                                         b
                                                                                         ((number-promise%-calculate c)))))))
                                      (%add-consistency-check (lambda () ((number-promise%-calculate promise)))) 
                                      #t)]
    [(list (? number-promise%? a)
           (? number-promise%? b)
           (? number-promise%? c))  (let ((promise (number-promise% (append (number-promise%-depends a)
                                                                            (number-promise%-depends b)
                                                                            (number-promise%-depends c))
                                                                    (lambda () (between? ((number-promise%-calculate a))
                                                                                         ((number-promise%-calculate b))
                                                                                         ((number-promise%-calculate c)))))))
                                      (%add-consistency-check (lambda () ((number-promise%-calculate promise)))) 
                                      #t)]
    ))

(define between
  (match-lambda*
    [(list (? number? a)
           (? number? b)
           (? number? c)) (if (and (>= a b)
                                   (<= b c))
                              b
                              (raise-result-error 'between
                                                  (format "not between ~a and ~a" a c)
                                                  b))]
    [(list (? number-promise%? a)
           (? number? b)
           (? number? c)) (number-promise% (number-promise%-depends a)
                                           (lambda () (between ((number-promise%-calculate a)) b c)))]
    [(list (? number? a)
           (? number-promise%? b)
           (? number? c)) (number-promise% (number-promise%-depends b)
                                           (lambda () (between a ((number-promise%-calculate b)) c)))]
    [(list (? number? a)
           (? number? b)
           (? number-promise%? c)) (number-promise% (number-promise%-depends c)
                                                    (lambda () (between a b ((number-promise%-calculate c)))))]
    [(list (? number-promise%? a)
           (? number? b)
           (? number-promise%? c)) (number-promise% (append (number-promise%-depends a)
                                                            (number-promise%-depends c))
                                                    (lambda () (between ((number-promise%-calculate a))
                                                                        b
                                                                        ((number-promise%-calculate c)))))]
    [(list (? number-promise%? a)
           (? number-promise%? b)
           (? number? c)) (number-promise% (append (number-promise%-depends a)
                                                   (number-promise%-depends b))
                                           (lambda () (between ((number-promise%-calculate a))
                                                               ((number-promise%-calculate b))
                                                               c)))]
    [(list (? number? a)
           (? number-promise%? b)
           (? number-promise%? c)) (number-promise% (append (number-promise%-depends b)
                                                            (number-promise%-depends c))
                                                    (lambda () (between a
                                                                        ((number-promise%-calculate b))
                                                                        ((number-promise%-calculate c)))))]
    
    [(list (? number-promise%? a)
           (? number-promise%? b)
           (? number-promise%? c)) (number-promise% (append (number-promise%-depends a)
                                                            (number-promise%-depends b)
                                                            (number-promise%-depends c))
                                                    (lambda () (between ((number-promise%-calculate a))
                                                                        ((number-promise%-calculate b))
                                                                        ((number-promise%-calculate c)))))]
    ))
