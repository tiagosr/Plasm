#lang racket

(define (asm-b n val)
  (bitwise-and 255 (arithmetic-shift val (* 8 n))))
(define big-endian #f)
(define (asm-write-byte b)
  ; substitute this for actual byte output in current section
  (printf "~X " (bitwise-and 255 (floor (inexact->exact b)))))
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

(struct %section
  (name
   start
   exports)
  #:transparent)
(define %sections (make-hash))


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

(define (65imm? n)
  (number? n))
(define (65addr? n)
  (number? n))
(define (65rel? n)
  (number? n))

(define @ 0)
(define (@@ n)
  (- n @))

(define (65imm op n)
  (db op n))
(define (65addr op n)
  (db op (asm-b 0 n) (asm-b 1 n)))
(define (65rel op n)
  (db op (@@ n)))


(%architecture
 '6502 #f
  (match-lambda
    [`(adc ,(? 65imm? n))        (65imm #x65 n)]
    [`(adc $,(? 65imm? n))       (65imm #x69 n)]
    [`(adc (,(? 65imm? n) x))    (65imm #x61 n)]
    [`(adc (,(? 65imm? n)) y)    (65imm #x71 n)]
    [`(adc (,(? 65imm? n)))      (65imm #x72 n)]
    [`(adc ,(? 65imm? n) x)      (65imm #x75 n)]
    [`(adc @,(? 65addr? n) x)    (65addr #x7D n)]
    [`(adc @,(? 65addr? n) y)    (65addr #x79 n)]
    [`(adc @,(? 65addr? n))      (65addr #x6D n)]
    
    [`(and ,(? 65imm? n))        (65imm #x25 n)]
    [`(and $,(? 65imm? n))       (65imm #x29 n)]
    [`(and (,(? 65imm? n) x))    (65imm #x21 n)]
    [`(and (,(? 65imm? n)) y)    (65imm #x31 n)]
    [`(and (,(? 65imm? n)))      (65imm #x32 n)]
    [`(and ,(? 65imm? n) x)      (65imm #x35 n)]
    [`(and @,(? 65addr? n) x)    (65addr #x3D n)]
    [`(and @,(? 65addr? n) y)    (65addr #x39 n)]
    [`(and @,(? 65addr? n))      (65addr #x2D n)]
    
    [`(asl a)                    (db #x0a)]
    [`(asl ,(? 65imm? n) x)      (db #x16 n)]
    [`(asl @,(? 65addr? n) x)    (65addr #x1e n)]
    [`(asl ,(? 65imm? n))        (db #x06 n)]
    [`(asl @,(? 65addr? n))      (65addr #x0e n)]
    
    [`(asl)                      (db #x0a)]
    
    [`(bcc ,(? 65rel? n))        (65rel #x90 n)]
    [`(bcs ,(? 65rel? n))        (65rel #xb0 n)]
    [`(beq ,(? 65rel? n))        (65rel #xf0 n)]
    [`(bmi ,(? 65rel? n))        (65rel #x30 n)]
    [`(bne ,(? 65rel? n))        (65rel #xd0 n)]
    [`(bpl ,(? 65rel? n))        (65rel #x10 n)]
    [`(bvc ,(? 65rel? n))        (65rel #x50 n)]
    [`(bvs ,(? 65rel? n))        (65rel #x70 n)]
    [`(bra ,(? 65rel? n))        (65rel #x80 n)]
    
    [`(bit ,(? 65imm? n))        (65imm #x24 n)]
    [`(bit $,(? 65imm? n))       (65imm #x89 n)]
    [`(bit (,(? 65imm? n)))      (65imm #x32 n)]
    [`(bit ,(? 65imm? n) x)      (65imm #x34 n)]
    [`(bit @,(? 65addr? n) x)    (65addr #x3C n)]
    [`(bit @,(? 65addr? n))      (65addr #x2C n)]

    [`(brk ,(? 65imm? n))        (65imm #x00 n)]
    [`(brk)                      (db 0)]

    [`(clc)                      (db #x18)]
    [`(cld)                      (db #xd8)]
    [`(cli)                      (db #x58)]
    [`(clv)                      (db #x68)]
      
    ))

(define (asm arch code)
  (let [(label-refs (look-for-labels code))]
    (display label-refs)))

