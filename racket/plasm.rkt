#lang racket

(struct %label
  (name
   pos)
  #:transparent)

(define @ 0)
(define @-labels
  (make-hasheq))
(struct %label-promise
  (depends
   calculate)
  #:transparent)

(struct %section
  (name
   start
   labels)
  #:transparent)
(define %sections (make-hash))
(define %consistency-checks (list))
(define (%add-consistency-check check)
  (set! %consistency-checks (append %consistency-checks (list check))))
(define (%check-consistency)
  (andmap (lambda (item)
            ((%label-promise-calculate item)))
          %consistency-checks))

(define %current-section (%section '<top> 0 (make-hash)))
(define (set-label label)
  (hash-set! (%section-labels %current-section) label (%label label @)))
(define (get-label label)
  (hash-ref (%section-labels %current-section) label (lambda () (%label-promise (list label) (lambda () (%label-pos (get-label label)))))))

(define-values (+a -a /a *a %or %and %xor %nand)
  (letrec
      ((mkop (lambda (op)
               (letrec ((p-op
                         (match-lambda* ; foldl applies arguments backwards
                           [(list (? number? a) (? number? b)) (op b a)]
                           [(list (? %label-promise? l) (? number? a)) (%label-promise (%label-promise-depends l)
                                                                                       (lambda () (op a
                                                                                                      ((%label-promise-calculate l)))))]
                           [(list (? number? a) (? %label-promise? l)) (%label-promise (%label-promise-depends l)
                                                                                       (lambda () (op ((%label-promise-calculate l))
                                                                                                      a)))]
                           [(list (? %label-promise? a) (? %label-promise? b))
                            (%label-promise (append (%label-promise-depends a) (%label-promise-depends b))
                                            (lambda () (op ((%label-promise-calculate b))
                                                           ((%label-promise-calculate a)))))])))
                 p-op))))
    (values (lambda args (foldl (mkop +) 0 args))
            (match-lambda* 
              [(list a) ((mkop -) 0 a)]
              [(list a rest ...) (foldl (mkop -) a rest)])
            (lambda args (foldl (mkop *) 1 args))
            (lambda args (foldl (mkop /) (car args) (cdr args)))
            (lambda args (foldl (mkop bitwise-ior) (car args) (cdr args)))
            (lambda args (foldl (mkop bitwise-and) (car args) (cdr args)))
            (lambda args (foldl (mkop bitwise-xor) (car args) (cdr args)))
            (lambda args (foldl (mkop (lambda (a b) (bitwise-not (bitwise-and a b)))) (car args) (cdr args)))
            )))

(define-values (%not %abs)
  (let ([mkop (lambda (op)
                (match-lambda
                  [(? number? i) (op i)]
                  [(? %label-promise? l) (%label-promise (%label-promise-depends l)
                                                         (lambda () (op ((%label-promise-calculate l)))))]
                  ))])
    (values (mkop bitwise-not)
            (mkop abs))))

(define-values (<< >>
                mod
                ><<-n ><<-b ><<-w ><<-d ><<-q
                >><-n >><-b >><-w >><-d >><-q)
  (letrec ([mkop (lambda (op)
                   (match-lambda*
                     [(list (? integer? a) (? integer? b)) (op a b)]
                     [(list (? %label-promise? a) (? integer? b))
                      (%label-promise (%label-promise-depends a)
                                      (lambda () (op ((%label-promise-calculate a)) b)))]
                     [(list (? integer? a) (? %label-promise? b))
                      (%label-promise (%label-promise-depends b)
                                      (lambda () (op a ((%label-promise-calculate b)))))]
                     [(list (? %label-promise? a) (? %label-promise? b))
                      (%label-promise (append (%label-promise-depends a) (%label-promise-depends b))
                                      (lambda () (op ((%label-promise-calculate a)) ((%label-promise-calculate b)))))]))]
           [%<< (mkop arithmetic-shift)]
           [%>> (mkop (lambda (a b) (arithmetic-shift (-a a) b)))]
           [mod (mkop modulo)]
           [mkrot (lambda (size mask op)
                    (lambda (a b)
                      (let ([~a (%and mask a)]
                            [r (mod b size)])
                        (%and mask (%or (op ~a r) (op ~a (+a (-a size) r)))))))])
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

(define-values (%< %> %= %<= %>= !=)
  (let ((mkop (lambda (op)
                   (match-lambda*
                     [(list (? number? a) (? number? b)) (op a b)]
                     [(list (? %label-promise? a) (? number? b))
                      (begin
                        (%add-consistency-check (%label-promise (%label-promise-depends a)
                                                                (lambda () (op ((%label-promise-calculate a)) b)))
                                                )
                        #t)]
                     [(list (? number? a) (? %label-promise? b))
                      (begin
                        (%add-consistency-check (%label-promise (%label-promise-depends b)
                                                                (lambda () (op a ((%label-promise-calculate b))))))
                        #t)]
                     [(list (? %label-promise? a) (? %label-promise? b))
                      (begin
                        (%add-consistency-check (%label-promise (append (%label-promise-depends a) (%label-promise-depends b))
                                                                (lambda () (op ((%label-promise-calculate a)) ((%label-promise-calculate b))))))
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
    [(list (? number? a) (? number? b) (? number? c))          (and (<= a b) (<= b c))]
    ; delay decision to consistency check in the other cases
    [(list (? %label-promise? a) (? number? b) (? number? c))  (if (>= b c)
                                                                   (let ((promise (%label-promise (%label-promise-depends a)
                                                                                                  (lambda () (<= a ((%label-promise-calculate a)))))))
                                                                     (%add-consistency-check (lambda () ((%label-promise-calculate promise)))) 
                                                                     #t)
                                                                   #f)]
    [(list (? number? a) (? number? b) (? %label-promise? c))  (if (<= a b)
                                                                   (let ((promise (%label-promise (%label-promise-depends c)
                                                                                                  (lambda () (>= b ((%label-promise-calculate c)))))))
                                                                     (%add-consistency-check (lambda () ((%label-promise-calculate promise)))) 
                                                                     #t)
                                                                   #f)]
    [(list (? number? a) (? %label-promise? b) (? number? c))  (let ((promise (%label-promise (%label-promise-depends b)
                                                                                              (lambda () (between? a ((%label-promise-calculate b)) c)))))
                                                                 (%add-consistency-check (lambda () ((%label-promise-calculate promise)))) 
                                                                 #t)]
    [(list (? %label-promise? a) (? %label-promise? b) (? number? c))  (let ((promise (%label-promise (append (%label-promise-depends a) (%label-promise-depends b))
                                                                                                      (lambda () (between? ((%label-promise-calculate a)) ((%label-promise-calculate b)) c)))))
                                                                         (%add-consistency-check (lambda () ((%label-promise-calculate promise)))) 
                                                                         #t)]
    [(list (? number? a) (? %label-promise? b) (? %label-promise? c))  (let ((promise (%label-promise (append (%label-promise-depends b) (%label-promise-depends c))
                                                                                                      (lambda () (between? a ((%label-promise-calculate b)) ((%label-promise-calculate c)))))))
                                                                         (%add-consistency-check (lambda () ((%label-promise-calculate promise)))) 
                                                                         #t)]
    [(list (? %label-promise? a) (? number? b) (? %label-promise? c))  (let ((promise (%label-promise (append (%label-promise-depends a) (%label-promise-depends c))
                                                                                                      (lambda () (between? ((%label-promise-calculate a)) b ((%label-promise-calculate c)))))))
                                                                         (%add-consistency-check (lambda () ((%label-promise-calculate promise)))) 
                                                                         #t)]
    [(list (? %label-promise? a) (? %label-promise? b) (? %label-promise? c))  (let ((promise (%label-promise (append (%label-promise-depends a) (%label-promise-depends b) (%label-promise-depends c))
                                                                                                              (lambda () (between? ((%label-promise-calculate a)) ((%label-promise-calculate b)) ((%label-promise-calculate c)))))))
                                                                                 (%add-consistency-check (lambda () ((%label-promise-calculate promise)))) 
                                                                                 #t)]
    ))

(define between
  (match-lambda*
    [(list (? number? a) (? number? b) (? number? c))         (if (and (>= a b) (<= b c))
                                                                  b
                                                                  (raise-result-error 'between (format "not between ~a and ~a" a c) b))]
    [(list (? %label-promise? a) (? number? b) (? number? c)) (%label-promise (%label-promise-depends a)
                                                                              (lambda () (between ((%label-promise-calculate a)) b c)))]
    [(list (? number? a) (? %label-promise? b) (? number? c)) (%label-promise (%label-promise-depends b)
                                                                              (lambda () (between a ((%label-promise-calculate b)) c)))]
    [(list (? number? a) (? number? b) (? %label-promise? c)) (%label-promise (%label-promise-depends c)
                                                                              (lambda () (between a b ((%label-promise-calculate c)))))]
    
    [(list (? %label-promise? a) (? number? b) (? %label-promise? c)) (%label-promise (append (%label-promise-depends a) (%label-promise-depends c))
                                                                                      (lambda () (between ((%label-promise-calculate a)) b ((%label-promise-calculate c)))))]
    [(list (? %label-promise? a) (? %label-promise? b) (? number? c)) (%label-promise (append (%label-promise-depends a) (%label-promise-depends b))
                                                                                      (lambda () (between ((%label-promise-calculate a)) ((%label-promise-calculate b)) c)))]
    [(list (? number? a) (? %label-promise? b) (? %label-promise? c)) (%label-promise (append (%label-promise-depends b) (%label-promise-depends c))
                                                                                      (lambda () (between a ((%label-promise-calculate b)) ((%label-promise-calculate c)))))]
    
    [(list (? %label-promise? a) (? %label-promise? b) (? %label-promise? c)) (%label-promise (append (%label-promise-depends a) (%label-promise-depends b) (%label-promise-depends c))
                                                                                              (lambda () (between ((%label-promise-calculate a)) ((%label-promise-calculate b)) ((%label-promise-calculate c)))))]
    ))
(define (@+ n)
  (set! @ (+a @ n)))
(define (@= n)
  (set! @ n))
(define (->@ n)
  (-a n @))

(define (aligned-w? val)
  (%= 0 (%and 1 val)))
(define (aligned-l? val)
  (%= 0 (%and 3 val)))

(define (asm-b n val)
  (%and 255 (<< val (*a 8 n))))
(define (asm-w n val)
  (%and #xffff (<< val (*a 16 n))))
(define (asm-d n val)
  (%and #xffffffff (<< val (*a 32 n))))
(define big-endian #f)

(define %bytes (list))
(define %big-endian% #f)
(define %promises (list))

(define (asm-write-byte b)
  (begin
    ;(printf "writing byte ~a at ~a~n" b @)
    (cond [(%label-promise? b)
           (let
               ([*@ @])
             (asm-write-byte 0)
             (set! %promises (append %promises (list (lambda () (begin (@= *@)
                                                                       (asm-write-byte ((%label-promise-calculate b)))))))))]
          [(%label? b) (asm-write-byte (%label-pos b))]
          [else (let
                    [(head (take %bytes @))
                     (tail (if (< @ (length %bytes))
                               (drop %bytes (+ 1 @))
                               '()))]
                  (set! %bytes (append head (list (%and 255 (floor (inexact->exact b)))) tail))
                  (@+ 1))])
    ))
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
    [(#t) (for-each asm-write-byte-list (map (lambda (n) (list (asm-b 1 n) (asm-b 0 n))) (asm-flatten rest)))]
    [(#f) (for-each asm-write-byte-list (map (lambda (n) (list (asm-b 0 n) (asm-b 1 n))) (asm-flatten rest)))]))
(define (dd . rest)
  (case %big-endian%
    [(#t) (for-each asm-write-byte-list (map (lambda (n) (list (asm-b 3 n) (asm-b 2 n) (asm-b 1 n) (asm-b 0 n))) (asm-flatten rest)))]
    [(#f) (for-each asm-write-byte-list (map (lambda (n) (list (asm-b 0 n) (asm-b 1 n) (asm-b 2 n) (asm-b 3 n))) (asm-flatten rest)))]))
(define (dq . rest)
  (case %big-endian%
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

(define (label? sym)
  (if (symbol? sym)
      #t
      #f))
(define (label-uncolon sym)
  (let ([str (symbol->string sym)])
    (if (eq? (last (string->list (symbol->string sym))) #\:)
        (string->symbol (substring str 0 (- (string-length str) 1)))
        str)))

(define (look-for-labels code)
  (filter symbol? (map (match-lambda
         [(? label? l) (label-uncolon l)]
         [`(set-label ,(? symbol? s)) s]
         [_ #f]) code)))

(struct %architecture
  (name
   big-endian
   recognizer))

(define %architectures (make-hash))
(define %current-architecture (void))

(define (make-architecture name endianess recognizer)
  (let [(arch (%architecture name endianess recognizer))]
    (hash-set! %architectures name arch)))

(define (label-code-inside-list l labels)
  (if (and (list? l)
           (< 1 (length l)))
      (append (list (car l)) (map (lambda (q) (if (member q labels)
                                                  (get-label q)
                                                  (label-code-inside-list q labels))) (cdr l)))
      l))
    
(define (label-code code labels)
  (map (match-lambda
         [(? label? lbl) `(set-label ',(label-uncolon lbl))]
         [rest `,rest]) (label-code-inside-list code labels)))

(define (asm-keyword thing ops)
  (match thing
    [(? label? lbl) (set-label `,lbl)]
    [op (ops op)]))

(define %asm-base
  (match-lambda
    [anything (eval anything)]))

(define %current-architecture% 'null)
(define (asm code)
  (let* [(old-big-endian %big-endian%)
         (labels (look-for-labels code))
         (labeled-code (label-code code labels))
         (big-endian (%architecture-big-endian (hash-ref %architectures %current-architecture%)))
         (recognizer (%architecture-recognizer (hash-ref %architectures %current-architecture%)))
         (old-bytes %bytes)
         (new-bytes (list))]
    (set! %promises (list))
    (set! %bytes new-bytes)
    (set! %big-endian% big-endian)
    (display labels)
    (for-each (lambda (op) (asm-keyword op recognizer)) labeled-code)
    (%check-consistency)
    (for-each (lambda (promise) (promise)) %promises)
    (set! %big-endian% old-big-endian)
    (set! new-bytes %bytes)
    (set! %bytes old-bytes)
    (values new-bytes)))

(make-architecture 'null #f %asm-base)

(define (asm-with-arch arch body)
  (let ((%old-arch% %current-architecture%)
        (dummy (set! %current-architecture% arch))
        (assembled (asm body)))
    (set! %current-architecture% %old-arch%)
    (values assembled)))


(provide (all-defined-out)
         (rename-out [+a +]
                     [-a -]
                     [*a *]
                     [/a /]
                     [%or ||]
                     [%and &]
                     [%xor ^]
                     [%not ~]
                     [%nand ~&]
                     [%< <]
                     [%> >]
                     [%= =]
                     [%<= <=]
                     [%>= >=]
                     ))