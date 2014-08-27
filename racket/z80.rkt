#lang racket

(require "plasm.rkt")

(define (zb? n)
  (number? n))

(define (zb op n)
  (db op n))

(define (zw? n)
  (number? n))
(define (za? n)
  (number? n))

(define (zw op n)
  (db op (asm-b 0 n) (asm-b 1 n)))

(define (zrel? n)
  (number? n))

(define (zrel op n)
  (db op (->@ n)))

(define (zed op)
  (db #xed op))
(define (zdd op)
  (db #xdd op))
(define (zddb op b)
  (db #xdd op b))
(define (zdds op i)
  (db #xdd op i))
(define (zddsb op i b)
  (db #xdd op i b))
(define (zfd op)
  (db #xfd op))
(define (zfdb op b)
  (db #xfd op b))
(define (zfds op i)
  (db #xfd op i))
(define (zfdsb op i b)
  (db #xfd op i b))

(define (between? upper n lower)
  (if (> n lower)
      (if (< n upper)
          #t
          #f)
      #f))
(define (zoff? i)
  (if (number? i)
      (if (between? -128 i 127)
          #t
          #f)
      #f))


(architecture
 'z80 #f
  (match-lambda
    [`(nop)       (db #x00)]
    
    [`(ld b b)                 (db   #x40)]
    [`(ld b c)                 (db   #x41)]
    [`(ld b d)                 (db   #x42)]
    [`(ld b e)                 (db   #x43)]
    [`(ld b h)                 (db   #x44)]
    [`(ld b l)                 (db   #x45)]
    [`(ld b (hl))              (db   #x46)]
    [`(ld b ixh)               (zdd  #x44)]
    [`(ld b ixl)               (zdd  #x45)]
    [`(ld b (ix ,(? zoff? s))) (zdds #x46 s)]
    [`(ld b iyh)               (zfd  #x44)]
    [`(ld b iyl)               (zfd  #x45)]
    [`(ld b (iy ,(? zoff? s))) (zfds #x46 s)]
    [`(ld b a)                 (db   #x47)]
    [`(ld b ,(? zb? n))        (zb   #x06 n)]
    
    [`(ld c b)                 (db   #x48)]
    [`(ld c c)                 (db   #x49)]
    [`(ld c d)                 (db   #x4a)]
    [`(ld c e)                 (db   #x4b)]
    [`(ld c h)                 (db   #x4c)]
    [`(ld c l)                 (db   #x4d)]
    [`(ld c (hl))              (db   #x4e)]
    [`(ld c ixh)               (zdd  #x4c)]
    [`(ld c ixl)               (zdd  #x4d)]
    [`(ld c (ix ,(? zoff? s))) (zdds #x4e s)]
    [`(ld c iyh)               (zfd  #x4c)]
    [`(ld c iyl)               (zfd  #x4d)]
    [`(ld c (iy ,(? zoff? s))) (zfds #x4e s)]
    [`(ld c a)                 (db   #x4f)]
    [`(ld c ,(? zb? n))        (zb   #x0e n)]
    
    [`(ld d b)                 (db   #x50)]
    [`(ld d c)                 (db   #x51)]
    [`(ld d d)                 (db   #x52)]
    [`(ld d e)                 (db   #x53)]
    [`(ld d h)                 (db   #x54)]
    [`(ld d l)                 (db   #x55)]
    [`(ld d (hl))              (db   #x56)]
    [`(ld d ixh)               (zdd  #x54)]
    [`(ld d ixl)               (zdd  #x55)]
    [`(ld d (ix ,(? zoff? s))) (zdds #x56 s)]
    [`(ld d iyh)               (zfd  #x54)]
    [`(ld d iyl)               (zfd  #x55)]
    [`(ld d (iy ,(? zoff? s))) (zfds #x56 s)]
    [`(ld d a)                 (db   #x57)]
    [`(ld d ,(? zb? n))        (zb   #x16 n)]
    
    [`(ld e b)                 (db   #x58)]
    [`(ld e c)                 (db   #x59)]
    [`(ld e d)                 (db   #x5a)]
    [`(ld e e)                 (db   #x5b)]
    [`(ld e h)                 (db   #x5c)]
    [`(ld e l)                 (db   #x5d)]
    [`(ld e (hl))              (db   #x5e)]
    [`(ld e ixh)               (zdd  #x5c)]
    [`(ld e ixl)               (zdd  #x5d)]
    [`(ld e (ix ,(? zoff? s))) (zdds #x5e s)]
    [`(ld e iyh)               (zfd  #x5c)]
    [`(ld e iyl)               (zfd  #x5d)]
    [`(ld e (iy ,(? zoff? s))) (zfds #x5e s)]
    [`(ld e a)                 (db   #x5f)]
    [`(ld e ,(? zb? n))        (zb   #x1e n)]
    
    [`(ld h b)                 (db   #x60)]
    [`(ld h c)                 (db   #x61)]
    [`(ld h d)                 (db   #x62)]
    [`(ld h e)                 (db   #x63)]
    [`(ld h h)                 (db   #x64)]
    [`(ld h l)                 (db   #x65)]
    [`(ld h (hl))              (db   #x66)]
    [`(ld h ixh)               (zdd  #x64)]
    [`(ld h ixl)               (zdd  #x65)]
    [`(ld h (ix ,(? zoff? s))) (zdds #x66 s)]
    [`(ld h iyh)               (zfd  #x64)]
    [`(ld h iyl)               (zfd  #x65)]
    [`(ld h (iy ,(? zoff? s))) (zfds #x66 s)]
    [`(ld h a)                 (db   #x67)]
    [`(ld h ,(? zb? n))        (zb   #x26 n)]
    
    [`(ld l b)                 (db   #x68)]
    [`(ld l c)                 (db   #x69)]
    [`(ld l d)                 (db   #x6a)]
    [`(ld l e)                 (db   #x6b)]
    [`(ld l h)                 (db   #x6c)]
    [`(ld l l)                 (db   #x6d)]
    [`(ld l (hl))              (db   #x6e)]
    [`(ld l ixh)               (zdd  #x6c)]
    [`(ld l ixl)               (zdd  #x6d)]
    [`(ld l (ix ,(? zoff? s))) (zdds #x6e s)]
    [`(ld l iyh)               (zfd  #x6c)]
    [`(ld l iyl)               (zfd  #x6d)]
    [`(ld l (iy ,(? zoff? s))) (zfds #x6e s)]
    [`(ld l a)                 (db   #x6f)]
    [`(ld l ,(? zb? n))        (zb   #x2e n)]
    
    [`(ld ixh b)          (zdd   #x60)]
    [`(ld ixh c)          (zdd   #x61)]
    [`(ld ixh d)          (zdd   #x62)]
    [`(ld ixh e)          (zdd   #x63)]
    [`(ld ixh h)          (zdd   #x64)]
    [`(ld ixh l)          (zdd   #x65)]
    [`(ld ixh (ix ,(? zrel? s))) (zdds #x66 s)]
    [`(ld ixh a)          (zdd   #x67)]
    [`(ld ixh ,(? zb? n)) (zddb #x26 n)]

    [`(ld iyh b)          (zfd  #x68)]
    [`(ld iyh c)          (zfd  #x69)]
    [`(ld iyh d)          (zfd  #x6a)]
    [`(ld iyh e)          (zfd  #x6b)]
    [`(ld iyh h)          (zfd  #x6c)]
    [`(ld iyh l)          (zfd  #x6d)]
    [`(ld iyh (iy ,(? zrel? s))) (zfds #x6e s)]
    [`(ld iyh a)          (zfd  #x6f)]
    [`(ld iyh ,(? zb? n)) (zfdb #x26 n)]
    
    [`(ld (hl) b)    (db #x70)]
    [`(ld (hl) c)    (db #x71)]
    [`(ld (hl) d)    (db #x72)]
    [`(ld (hl) e)    (db #x73)]
    [`(ld (hl) h)    (db #x74)]
    [`(ld (hl) l)    (db #x75)]
    [`(halt)         (db #x76)]
    [`(ld (hl) a)    (db #x77)]
    
    [`(ld a b)    (db #x78)]
    [`(ld a c)    (db #x79)]
    [`(ld a d)    (db #x7a)]
    [`(ld a e)    (db #x7b)]
    [`(ld a h)    (db #x7c)]
    [`(ld a l)    (db #x7d)]
    [`(ld a (hl)) (db #x7e)]
    [`(ld a ixh)    (zdd #x7c)]
    [`(ld a ixl)    (zdd #x7d)]
    [`(ld a (ix ,(? zoff? s))) (zdds #x7e s)]
    [`(ld a iyh)    (zfd #x7c)]
    [`(ld a iyl)    (zfd #x7d)]
    [`(ld a (iy ,(? zoff? s))) (zfds #x7e s)]
    [`(ld a a)    (db #x7f)]
    [`(ld a (bc)) (db #x0a)]
    [`(ld a (de)) (db #x1a)]
    [`(ld a (,(? za? n))) (zw #x3a n)]
    [`(ld a ,(? zb? n))   (zb #x3e n)]

    [`(add a b)    (db #xa0)]
    [`(add a c)    (db #xa1)]
    [`(add a d)    (db #xa2)]
    [`(add a e)    (db #xa3)]
    [`(add a h)    (db #xa4)]
    [`(add a l)    (db #xa5)]
    [`(add a (hl)) (db #xa6)]
    [`(add a a)    (db #xa7)]
    
    [`(adc a b)    (db #x88)]
    [`(adc a c)    (db #x89)]
    [`(adc a d)    (db #x8a)]
    [`(adc a e)    (db #x8b)]
    [`(adc a h)    (db #x8c)]
    [`(adc a l)    (db #x8d)]
    [`(adc a (hl)) (db #x8e)]
    [`(adc a a)    (db #x8f)]

    [`(sub b)      (db #x90)]
    [`(sub c)      (db #x91)]
    [`(sub d)      (db #x92)]
    [`(sub e)      (db #x93)]
    [`(sub h)      (db #x94)]
    [`(sub l)      (db #x95)]
    [`(sub (hl))   (db #x96)]
    [`(sub a)      (db #x97)]
    
    [`(sbc a b)    (db #x98)]
    [`(sbc a c)    (db #x99)]
    [`(sbc a d)    (db #x9a)]
    [`(sbc a e)    (db #x9b)]
    [`(sbc a h)    (db #x9c)]
    [`(sbc a l)    (db #x9d)]
    [`(sbc a (hl)) (db #x9e)]
    [`(sbc a a)    (db #x9f)]

    [`(and b)      (db #xa0)]
    [`(and c)      (db #xa1)]
    [`(and d)      (db #xa2)]
    [`(and e)      (db #xa3)]
    [`(and h)      (db #xa4)]
    [`(and l)      (db #xa5)]
    [`(and (hl))   (db #xa6)]
    [`(and a)      (db #xa7)]
    
    [`(xor b)      (db #xa8)]
    [`(xor c)      (db #xa9)]
    [`(xor d)      (db #xaa)]
    [`(xor e)      (db #xab)]
    [`(xor h)      (db #xac)]
    [`(xor l)      (db #xad)]
    [`(xor (hl))   (db #xae)]
    [`(xor a)      (db #xaf)]

    [`(or b)       (db #xb0)]
    [`(or c)       (db #xb1)]
    [`(or d)       (db #xb2)]
    [`(or e)       (db #xb3)]
    [`(or h)       (db #xb4)]
    [`(or l)       (db #xb5)]
    [`(or (hl))    (db #xb6)]
    [`(or a)       (db #xb7)]
    
    [`(cp b)       (db #xb8)]
    [`(cp c)       (db #xb9)]
    [`(cp d)       (db #xba)]
    [`(cp e)       (db #xbb)]
    [`(cp h)       (db #xbc)]
    [`(cp l)       (db #xbd)]
    [`(cp (hl))    (db #xbe)]
    [`(cp a)       (db #xbf)]

    [`(ret nz)     (db #xc0)]
    [`(ret z)      (db #xc8)]
    [`(ret nc)     (db #xd0)]
    [`(ret c)      (db #xd8)]
    [`(ret pe)     (db #xe0)]
    [`(ret po)     (db #xe8)]
    [`(ret p)      (db #xc0)]
    [`(ret m)      (db #xc0)]

    [`(pop bc)     (db #xc1)]
    [`(pop de)     (db #xd1)]
    [`(pop hl)     (db #xe1)]
    [`(pop af)     (db #xf1)]

    [`(ret)        (db #xc9)]
    [`(exx)        (db #xd9)]
    [`(jp (hl))    (db #xe9)]
    [`(ld sp hl)   (db #xf9)]

    [`(ld bc ,(? zw? n))            (zw #x01 n)]
    [`(ld (bc) a)                     (db #x02)]
    [`(inc bc)                        (db #x03)]
    [`(inc b)                         (db #x04)]
    [`(dec b)                         (db #x05)]
    [`(rlca)                          (db #x07)]
    [`(ex af af*)                     (db #x08)]
    [`(add hl bc)                     (db #x09)]
    [`(ld a (bc))                     (db #x0a)]
    [`(dec bc)                        (db #x0b)]
    [`(inc c)                         (db #x0c)]
    [`(dec c)                         (db #x0d)]
    [`(rrca)                          (db #x0f)]
    
    [`(djnz ,(? zrel? n))           (zrel #x10 n)]
    [`(ld de ,(? zw? n))            (zw #x11 n)]
    [`(ld (de) a)                     (db #x12)]
    [`(inc de)                        (db #x13)]
    [`(inc d)                         (db #x14)]
    [`(dec d)                         (db #x15)]
    
    [`(jp ,(? za? n))    (zw #xc3 n)]
    [`(jp nz ,(? za? n)) (zw #xc2 n)]
    [`(jp z ,(? za? n))  (zw #xca n)]
    [`(jp nc ,(? za? n)) (zw #xd2 n)]
    [`(jp c ,(? za? n))  (zw #xda n)]
    [`(jp po ,(? za? n)) (zw #xe2 n)]
    [`(jp pe ,(? za? n)) (zw #xea n)]
    [`(jp p ,(? za? n))  (zw #xf2 n)]
    [`(jp m ,(? za? n))  (zw #xfa n)]
    [`(jp (hl))          (db #xe9)]
    [`(jp (ix))          (zdd #xe9)]
    [`(jp (iy))          (zfd #xe9)]
    
    [`(djnz ,(? zrel? n))  (zb #x10 n)]
    [`(jr ,(? zrel? n))    (zb #x18 n)]
    [`(jr nz ,(? zrel? n)) (zb #x20 n)]
    [`(jr z ,(? zrel? n))  (zb #x28 n)]
    [`(jr nc ,(? zrel? n)) (zb #x30 n)]
    [`(jr c ,(? zrel? n))  (zb #x38 n)]
    
    [`(call ,(? za? n))    (zw #xcd n)]
    [`(call nz ,(? za? n)) (zw #xc4 n)]
    [`(call z ,(? za? n))  (zw #xcc n)]
    [`(call nc ,(? za? n)) (zw #xd4 n)]
    [`(call c ,(? za? n))  (zw #xdc n)]
    [`(call po ,(? za? n)) (zw #xe4 n)]
    [`(call pe ,(? za? n)) (zw #xec n)]
    [`(call p ,(? za? n))  (zw #xf4 n)]
    [`(call m ,(? za? n))  (zw #xfc n)]
    
    [`(ret)                (db #xc9)]
    [`(ret nz)               (db #xc0)]
    [`(ret z)                (db #xc8)]
    [`(ret nc)               (db #xd0)]
    [`(ret c)                (db #xd8)]
    [`(ret po)               (db #xe0)]
    [`(ret pe)               (db #xe8)]
    [`(ret p)                (db #xf0)]
    [`(ret p)                (db #xf8)]
    
    [`(reti) (db #xed #x4d)]
    [`(retn) (db #xed #x45)]
    
    [`(rst 0)    (db #xc7)]
    [`(rst 8)    (db #xcf)]
    [`(rst #x10) (db #xd7)]
    [`(rst #x18) (db #xdf)]
    [`(rst #x20) (db #xe7)]
    [`(rst #x28) (db #xef)]
    [`(rst #x30) (db #xf7)]
    [`(rst #x38) (db #xff)]
    
    [`(push bc)  (db  #xc5)]
    [`(push de)  (db  #xd5)]
    [`(push hl)  (db  #xe5)]
    [`(push af)  (db  #xf5)]
    [`(push ix)  (zdd #xe5)]
    [`(push iy)  (zfd #xe5)]
    
    [`(pop bc)  (db  #xc1)]
    [`(pop de)  (db  #xd1)]
    [`(pop hl)  (db  #xe1)]
    [`(pop af)  (db  #xf1)]
    [`(pop ix)  (zdd #xe1)]
    [`(pop iy)  (zfd #xe1)]
    
    ))