#lang racket

(require racket/promise)

(struct %label
  (name
   pos)
  #:transparent)

(struct %section
  (name
   start
   labels)
  #:transparent)
(define %sections (make-hash))

(define (asm-b n val)
  (bitwise-and 255 (arithmetic-shift val (* 8 n))))
(define big-endian #f)

(define @ 0)
(define (@+ n)
  (set! @ (+ @ n)))
(define (@= n)
  (set! @ n))
(define (->@ n)
  (- n @))

(define (asm-write-byte b)
  ; substitute this for actual byte output in current section
  (begin
    (printf "~X " (bitwise-and 255 (floor (inexact->exact b))))
    (@+ 1)))
(define (asm-write-byte-list l)
  (for-each asm-write-byte l))

(define (asm-flatten l)
  (flatten (map (λ (i) (cond
                              [(string? i) (map char->integer (string->list i))]
                              [else i])) l)))

(define (db . rest)
  (asm-write-byte-list (asm-flatten rest)))
(define (dw . rest)
  (case big-endian
    [(#t) (for-each asm-write-byte-list (map (lambda (n) (list (asm-b 1 n) (asm-b 0 n))) (asm-flatten rest)))]
    [(#f) (for-each asm-write-byte-list (map (lambda (n) (list (asm-b 0 n) (asm-b 1 n))) (asm-flatten rest)))]))
(define (dd . rest)
  (case big-endian
    [(#t) (for-each asm-write-byte-list (map (lambda (n) (list (asm-b 3 n) (asm-b 2 n) (asm-b 1 n) (asm-b 0 n))) (asm-flatten rest)))]
    [(#f) (for-each asm-write-byte-list (map (lambda (n) (list (asm-b 0 n) (asm-b 1 n) (asm-b 2 n) (asm-b 3 n))) (asm-flatten rest)))]))
(define (dq . rest)
  (case big-endian
    [(#t) (for-each asm-write-byte-list (map (lambda (n) (list (asm-b 7 n) (asm-b 6 n) (asm-b 5 n) (asm-b 4 n)
                                                               (asm-b 3 n) (asm-b 2 n) (asm-b 1 n) (asm-b 0 n))) (asm-flatten rest)))]
    [(#f) (for-each asm-write-byte-list (map (lambda (n) (list (asm-b 0 n) (asm-b 1 n) (asm-b 2 n) (asm-b 3 n)
                                                               (asm-b 4 n) (asm-b 5 n) (asm-b 6 n) (asm-b 7 n))) (asm-flatten rest)))]))
(define (dsb count value)
  (for-each db (for/list ([i count]) value)))
(define (dsw count value)
  (for-each dw (for/list ([i count]) value)))
(define (dsd count value)
  (for-each dd (for/list ([i count]) value)))
(define (dsq count value)
  (for-each dq (for/list ([i count]) value)))

(define (dbrnd count low high)
  (for-each db (for/list ([i count]) (+ low (random (- high low))))))
(define (dwrnd count low high)
  (for-each dw (for/list ([i count]) (+ low (random (- high low))))))
(define (ddrnd count low high)
  (for-each dd (for/list ([i count]) (+ low (random (- high low))))))
(define (dqrnd count low high)
  (for-each dq (for/list ([i count]) (+ low (random (- high low))))))

(define (bcd-digit digit number)
  (let ((p (expt 10 digit)))
    (remainder (quotient number p) 10)))
(define (dbcdb number)
  (db (+ (* (bcd-digit 1 number) 16) (bcd-digit 0 number))))

(define (asm-angle-list-op write op)
  (lambda (angle count angle-step scale offset)
    (for-each write (for/list ([i count]) (+ offset (* scale (op (+ angle (* i angle-step)))))))))
(define dbcos (asm-angle-list-op db cos))
(define dwcos (asm-angle-list-op dw cos))
(define ddcos (asm-angle-list-op dd cos))
(define dqcos (asm-angle-list-op dq cos))
(define dbsin (asm-angle-list-op db sin))
(define dwsin (asm-angle-list-op dw sin))
(define ddsin (asm-angle-list-op dd sin))
(define dqsin (asm-angle-list-op dq sin))

(struct %struct-member
  (name %struct params)
  #:transparent)
(struct %struct-def
  (sizer
   members)
  #:transparent)

; prepare the structs hash with default values
(define %structs (make-hash))
(hash-set! %structs 'db (%struct-def (λ () 1) '()))
(hash-set! %structs 'dw (%struct-def (λ () 2) '()))
(hash-set! %structs 'dd (%struct-def (λ () 4) '()))
(hash-set! %structs 'dq (%struct-def (λ () 8) '()))
(hash-set! %structs 'dsb (%struct-def (λ (count) count) '()))
(hash-set! %structs 'dsw (%struct-def (λ (count) (* 2 count)) '()))
(hash-set! %structs 'dsd (%struct-def (λ (count) (* 4 count)) '()))
(hash-set! %structs 'dsq (%struct-def (λ (count) (* 8 count)) '()))

; syntax for declaring new structs
; (%struct oe
;    ((member other-struct)
;     ...))
(define-syntax %struct
  (syntax-rules ()
    [(_ name
        ((member %struct-name params ...) ...))
     (letrec [(members ; make list of struct members
               (list (%struct-member 'member (hash-ref %structs '%struct-name) (list params ...)) ...))
              (%s-def ; define struct, folding the members list accumulating the sizes of members
               (%struct-def (foldl (λ (m s) 
                                     (let [(%m (%struct-member-%struct m))]
                                       (+ s (apply (%struct-def-sizer %m) (%struct-member-params m)))))
                                   0 members)
                            members))]
       ; maybe deal with struct redefinitions here?
         (hash-set! %structs 'name %s-def))]))



(define-syntax section
  (syntax-rules ()
    [(_ name
        ((statement ...) ...))
     (let [(s (make-%section 'name 0 (list)))]
       (begin
         (statement ...) ...
         (hash-set! %sections 'name s)))]))

(define-syntax ram-section
  (syntax-rules ()
    [(_ name
        ((statement ...) ...))
     (let [(s (make-%section 'name 0 (list)))]
       (begin
         (statement ...) ...
         (hash-set! %sections 'name s)))]))


;(define-syntax (memory-map stx)
;  (syntax-parse stx
;    [(memory-map (~optional (~seq


(define (label? sym)
  (if (symbol? sym)
      (if (eq? (last (string->list (symbol->string sym))) #\:)
          #t #f)
      #f))


(define (set-label sym)
  sym)

(define (get-label sym)
  sym)
  

(define (look-for-labels code)
  (filter (lambda (statement)
            (cond
              [(label? statement) #t]
              ; test for sections here
              [else #f])) code))

(struct %architecture
  (name
   big-endian
   recognizer))

(define %architectures (make-hash))

(define (architecture name endianess recognizer)
  (let [(arch (%architecture name endianess recognizer))]
    (hash-set! %architectures name arch)))


(define (label-code code labels)
  code)
(define (asm-keyword thing ops)
  (match thing
    [(? label? label) (set-label label)]
    [op (ops op)]))

(define %asm-base
  (match-lambda
    [anything (eval anything)]))

(define (asm arch code)
  (let* [(label-refs (look-for-labels code))
         (labeled-code (label-code code label-refs))
         (recognizer (%architecture-recognizer (hash-ref %architectures arch)))]
    (for-each (lambda (op) (asm-keyword op recognizer)) labeled-code)))

(provide (all-defined-out))