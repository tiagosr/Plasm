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
(define (asm-w n val)
  (bitwise-and #xffff (arithmetic-shift val (* 16 n))))
(define (asm-d n val)
  (bitwise-and #xffffffff (arithmetic-shift val (* 32 n))))`
(define big-endian #f)

(define @ 0)
(define (@+ n)
  (set! @ (+ @ n)))
(define (@= n)
  (set! @ n))
(define (->@ n)
  (- n @))
(define @-labels
  (make-hasheq))
(define %bytes (open-output-bytes))
(define %assembling% #f)
(define %big-endian% #f)
(define (asm-write-byte b)
  (begin
    (if %assembling%
        (write-byte (bitwise-and 255 (floor (inexact->exact b))) %bytes)
        #f)
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
  (case %big-endian%
    [(#t) (for-each asm-write-byte-list (map (λ (n) (list (asm-b 1 n) (asm-b 0 n))) (asm-flatten rest)))]
    [(#f) (for-each asm-write-byte-list (map (λ (n) (list (asm-b 0 n) (asm-b 1 n))) (asm-flatten rest)))]))
(define (dd . rest)
  (case %big-endian%
    [(#t) (for-each asm-write-byte-list (map (λ (n) (list (asm-b 3 n) (asm-b 2 n) (asm-b 1 n) (asm-b 0 n))) (asm-flatten rest)))]
    [(#f) (for-each asm-write-byte-list (map (λ (n) (list (asm-b 0 n) (asm-b 1 n) (asm-b 2 n) (asm-b 3 n))) (asm-flatten rest)))]))
(define (dq . rest)
  (case %big-endian%
    [(#t) (for-each asm-write-byte-list (map (λ (n) (list (asm-b 7 n) (asm-b 6 n) (asm-b 5 n) (asm-b 4 n)
                                                          (asm-b 3 n) (asm-b 2 n) (asm-b 1 n) (asm-b 0 n))) (asm-flatten rest)))]
    [(#f) (for-each asm-write-byte-list (map (λ (n) (list (asm-b 0 n) (asm-b 1 n) (asm-b 2 n) (asm-b 3 n)
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
(define (unlabelize sym)
  (let ([str (symbol->string sym)])
    (string->symbol (substring str 0 (- (string-length str) 1)))))

(define (set-label sym)
  (hash-set! @-labels sym @))

(define (get-label sym)
  (hash-ref! @-labels sym 0))

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
  (map (match-lambda
         [(? label? lbl) `(set-label ',(unlabelize lbl))]
         [rest `,rest]) code))

(define (asm-keyword thing ops)
  (match thing
    [(? label? lbl) (set-label `,lbl)]
    [op (ops op)]))

(define %asm-base
  (match-lambda
    [anything (eval anything)]))

(define (asm arch code)
  (let* [(old-big-endian %big-endian%)
         (labels (look-for-labels code))
         (labeled-code (label-code code labels))
         (big-endian (%architecture-big-endian (hash-ref %architectures arch)))
         (recognizer (%architecture-recognizer (hash-ref %architectures arch)))
         (was-assembling %assembling%)
         (~@ @)
         (old-bytes %bytes)
         (new-bytes (open-output-bytes))]
    (set! %bytes new-bytes)
    (set! %big-endian% big-endian)
    (set! %assembling% #f)
    (for-each (lambda (op) (asm-keyword op recognizer)) labeled-code)
    (set! %assembling% #t)
    (@= ~@)
    (for-each (lambda (op) (asm-keyword op recognizer)) labeled-code)
    (set! %assembling% was-assembling)
    (set! %big-endian% old-big-endian)
    (set! %bytes old-bytes)
    (get-output-bytes new-bytes)))

(architecture 'null #f %asm-base)
(provide (all-defined-out))