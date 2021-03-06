#lang racket

(require "plasm.rkt")

(define (zb? n)   (between? -128 n 255))
(define (zw? n)   (between? -32768 n 65535))
(define (za? n)   (between? 0 n 65535))
(define (zbit? n) (between? 0 n 7))
(define (zrel? n) (between? -128 (->@ n) 127))

(define (zb op n) (db op n))
(define (zw op n) (db op (asm-b 0 n) (asm-b 1 n)))
(define (zrel op n) (db op (->@ n)))

(define (zed op)
  (db #xed op))
(define (zedw op b)
  (db #xed op (asm-b 0 b) (asm-b 1 b)))

(define (zcb op)
  (db #xcb op))
(define (zdd op)
  (db #xdd op))
(define (zddb op b)
  (db #xdd op b))
(define (zddw op b)
  (db #xdd op (asm-b 0 b) (asm-b 1 b)))
(define (zdds op i)
  (db #xdd op i))
(define (zddsb op i b)
  (db #xdd op i b))
(define (zfd op)
  (db #xfd op))
(define (zfdb op b)
  (db #xfd op b))
(define (zfdw op b)
  (db #xfd op (asm-b 0 b) (asm-b 1 b)))
(define (zfds op i)
  (db #xfd op i))
(define (zfdsb op i b)
  (db #xfd op i b))

(define (zddcbs op i)
  (db #xdd #xcb i op))
(define (zfdcbs op i)
  (db #xfd #xcb i op))

(define (zoff? i)
  (between? -128 i 127))


(define %z80-common
  (match-lambda
    [`(nop)       (db #x00)]
    
    [`(ld b b)                 (db   #x40)]
    [`(ld b c)                 (db   #x41)]
    [`(ld b d)                 (db   #x42)]
    [`(ld b e)                 (db   #x43)]
    [`(ld b h)                 (db   #x44)]
    [`(ld b l)                 (db   #x45)]
    [`(ld b (hl))              (db   #x46)]
    [`(ld b a)                 (db   #x47)]
    [`(ld b ,(? zb? n))        (zb   #x06 n)]
    
    [`(ld c b)                 (db   #x48)]
    [`(ld c c)                 (db   #x49)]
    [`(ld c d)                 (db   #x4a)]
    [`(ld c e)                 (db   #x4b)]
    [`(ld c h)                 (db   #x4c)]
    [`(ld c l)                 (db   #x4d)]
    [`(ld c (hl))              (db   #x4e)]
    [`(ld c a)                 (db   #x4f)]
    [`(ld c ,(? zb? n))        (zb   #x0e n)]
    
    [`(ld d b)                 (db   #x50)]
    [`(ld d c)                 (db   #x51)]
    [`(ld d d)                 (db   #x52)]
    [`(ld d e)                 (db   #x53)]
    [`(ld d h)                 (db   #x54)]
    [`(ld d l)                 (db   #x55)]
    [`(ld d (hl))              (db   #x56)]
    [`(ld d a)                 (db   #x57)]
    [`(ld d ,(? zb? n))        (zb   #x16 n)]
    
    [`(ld e b)                 (db   #x58)]
    [`(ld e c)                 (db   #x59)]
    [`(ld e d)                 (db   #x5a)]
    [`(ld e e)                 (db   #x5b)]
    [`(ld e h)                 (db   #x5c)]
    [`(ld e l)                 (db   #x5d)]
    [`(ld e (hl))              (db   #x5e)]
    [`(ld e a)                 (db   #x5f)]
    [`(ld e ,(? zb? n))        (zb   #x1e n)]
    
    [`(ld h b)                 (db   #x60)]
    [`(ld h c)                 (db   #x61)]
    [`(ld h d)                 (db   #x62)]
    [`(ld h e)                 (db   #x63)]
    [`(ld h h)                 (db   #x64)]
    [`(ld h l)                 (db   #x65)]
    [`(ld h (hl))              (db   #x66)]
    [`(ld h a)                 (db   #x67)]
    [`(ld h ,(? zb? n))        (zb   #x26 n)]
    
    [`(ld l b)                 (db   #x68)]
    [`(ld l c)                 (db   #x69)]
    [`(ld l d)                 (db   #x6a)]
    [`(ld l e)                 (db   #x6b)]
    [`(ld l h)                 (db   #x6c)]
    [`(ld l l)                 (db   #x6d)]
    [`(ld l (hl))              (db   #x6e)]
    [`(ld l a)                 (db   #x6f)]
    [`(ld l ,(? zb? n))        (zb   #x2e n)]
    
    [`(ld (hl) b)                 (db #x70)]
    [`(ld (hl) c)                 (db #x71)]
    [`(ld (hl) d)                 (db #x72)]
    [`(ld (hl) e)                 (db #x73)]
    [`(ld (hl) h)                 (db #x74)]
    [`(ld (hl) l)                 (db #x75)]
    [`(halt)                      (db #x76)]
    [`(ld (hl) a)                 (db #x77)]
    
    [`(ld a b)                 (db   #x78)]
    [`(ld a c)                 (db   #x79)]
    [`(ld a d)                 (db   #x7a)]
    [`(ld a e)                 (db   #x7b)]
    [`(ld a h)                 (db   #x7c)]
    [`(ld a l)                 (db   #x7d)]
    [`(ld a (hl))              (db   #x7e)]
    [`(ld a a)                 (db   #x7f)]
    [`(ld a (bc))              (db   #x0a)]
    [`(ld a (de))              (db   #x1a)]
    [`(ld a (,(? za? n)))      (zw   #x3a n)]
    [`(ld a ,(? zb? n))        (zb   #x3e n)]
    
    [`(ld (bc) a)          (db #x02)]
    [`(ld (de) a)          (db #x12)]
    [`(ld (,(? zw? w)) a)  (zw #x32 w)]
    
    [`(ld bc ,(? zw? w))   (zw #x01 w)]
    [`(ld de ,(? zw? w))   (zw #x11 w)]
    [`(ld hl ,(? zw? w))   (zw #x21 w)]
    [`(ld sp ,(? zw? w))   (zw #x31 w)]
    
    [`(ld bc (,(? za? w))) (zedw #x4b w)]
    [`(ld de (,(? za? w))) (zedw #x5b w)]
    [`(ld sp (,(? za? w))) (zedw #x7b w)]
    
    [`(ld (,(? za? w)) bc) (zedw #x43 w)]
    [`(ld (,(? za? w)) de) (zedw #x53 w)]
    [`(ld (,(? za? w)) sp) (zedw #x73 w)]
    
    [`(ld sp hl)   (db #xf9)]
    [`(ld sp ix)   (zdd #xf9)]
    [`(ld sp iy)   (zfd #xf9)]
    
    
    [`(add a b)    (db #xa0)]
    [`(add a c)    (db #xa1)]
    [`(add a d)    (db #xa2)]
    [`(add a e)    (db #xa3)]
    [`(add a h)    (db #xa4)]
    [`(add a l)    (db #xa5)]
    [`(add a (hl)) (db #xa6)]
    [`(add a a)    (db #xa7)]
    [`(add a ,(? zb? n)) (zb #xc6 n)]
    [`(add a ixh)             (zdd  #xa4)]
    [`(add a ixl)             (zdd  #xa5)]
    [`(add a (ix ,(? zoff? n))) (zdds #xa6 n)]
    [`(add a iyh)             (zfd  #xa4)]
    [`(add a iyl)             (zfd  #xa5)]
    [`(add a (iy ,(? zoff? n))) (zfds #xa6 n)]
    
    [`(add hl bc)  (db #x09)]
    [`(add hl de)  (db #x19)]
    [`(add hl hl)  (db #x29)]
    [`(add hl sp)  (db #x39)]
    [`(add ix bc)  (zdd #x09)]
    [`(add ix de)  (zdd #x19)]
    [`(add ix hl)  (zdd #x29)]
    [`(add ix sp)  (zdd #x39)]
    [`(add iy bc)  (zfd #x09)]
    [`(add iy de)  (zfd #x19)]
    [`(add iy hl)  (zfd #x29)]
    [`(add iy sp)  (zfd #x39)]
    
    [`(adc a b)    (db #x88)]
    [`(adc a c)    (db #x89)]
    [`(adc a d)    (db #x8a)]
    [`(adc a e)    (db #x8b)]
    [`(adc a h)    (db #x8c)]
    [`(adc a l)    (db #x8d)]
    [`(adc a (hl)) (db #x8e)]
    [`(adc a a)    (db #x8f)]
    [`(adc a ,(? zb? n)) (zb #xce n)]
    [`(adc hl bc)  (zed #x4a)]
    [`(adc hl de)  (zed #x5a)]
    [`(adc hl hl)  (zed #x6a)]
    [`(adc hl sp)  (zed #x7a)]
    [`(adc a ixh)             (zdd  #x8c)]
    [`(adc a ixl)             (zdd  #x8d)]
    [`(adc a (ix ,(? zoff? n))) (zdds #x8e n)]
    [`(adc a iyh)             (zfd  #x8c)]
    [`(adc a iyl)             (zfd  #x8d)]
    [`(adc a (iy ,(? zoff? n))) (zfds #x8e n)]
    
    [`(sub b)      (db #x90)]
    [`(sub c)      (db #x91)]
    [`(sub d)      (db #x92)]
    [`(sub e)      (db #x93)]
    [`(sub h)      (db #x94)]
    [`(sub l)      (db #x95)]
    [`(sub (hl))   (db #x96)]
    [`(sub a)      (db #x97)]
    [`(sub ,(? zb? n)) (zb #xd6 n)]
    [`(sub ixh)      (zdd #x94)]
    [`(sub ixl)      (zdd #x95)]
    [`(sub (ix ,(? zoff? s)))   (zdds #x96 s)]
    [`(sub iyh)      (zfd #x94)]
    [`(sub iyl)      (zfd #x95)]
    [`(sub (iy ,(? zoff? s)))   (zfds #x96 s)]
    
    
    [`(sbc a b)    (db #x98)]
    [`(sbc a c)    (db #x99)]
    [`(sbc a d)    (db #x9a)]
    [`(sbc a e)    (db #x9b)]
    [`(sbc a h)    (db #x9c)]
    [`(sbc a l)    (db #x9d)]
    [`(sbc a (hl)) (db #x9e)]
    [`(sbc a a)    (db #x9f)]
    [`(sbc a ixh)    (zdd #x9c)]
    [`(sbc a ixl)    (zdd #x9d)]
    [`(sbc a (ix ,(? zoff? s))) (zdds #x9e)]
    [`(sbc a iyh)    (zfd #x9c)]
    [`(sbc a iyl)    (zfd #x9d)]
    [`(sbc a (iy ,(? zoff? s))) (zfds #x9e)]
    [`(sbc a ,(? zb? n)) (zb #xde n)]
    
    [`(and b)      (db #xa0)]
    [`(and c)      (db #xa1)]
    [`(and d)      (db #xa2)]
    [`(and e)      (db #xa3)]
    [`(and h)      (db #xa4)]
    [`(and l)      (db #xa5)]
    [`(and (hl))   (db #xa6)]
    [`(and a)      (db #xa7)]
    [`(and ixh)             (zdd  #xa4)]
    [`(and ixl)             (zdd  #xa5)]
    [`(and (ix ,(? zoff? n))) (zdds #xa6 n)]
    [`(and iyh)             (zfd  #xa4)]
    [`(and iyl)             (zfd  #xa5)]
    [`(and (iy ,(? zoff? n))) (zfds #xa6 n)]
    [`(and ,(? zb? n))      (zb   #xe6 n)]
    
    [`(xor b)      (db #xa8)]
    [`(xor c)      (db #xa9)]
    [`(xor d)      (db #xaa)]
    [`(xor e)      (db #xab)]
    [`(xor h)      (db #xac)]
    [`(xor l)      (db #xad)]
    [`(xor (hl))   (db #xae)]
    [`(xor a)      (db #xaf)]
    [`(xor ixh)             (zdd  #xac)]
    [`(xor ixl)             (zdd  #xad)]
    [`(xor (ix ,(? zoff? n))) (zdds #xae n)]
    [`(xor iyh)             (zfd  #xac)]
    [`(xor iyl)             (zfd  #xad)]
    [`(xor (iy ,(? zoff? n))) (zfds #xae n)]
    [`(xor ,(? zb? n))      (zb   #xee n)]
    
    [`(or b)       (db #xb0)]
    [`(or c)       (db #xb1)]
    [`(or d)       (db #xb2)]
    [`(or e)       (db #xb3)]
    [`(or h)       (db #xb4)]
    [`(or l)       (db #xb5)]
    [`(or (hl))    (db #xb6)]
    [`(or a)       (db #xb7)]
    [`(or ixh)             (zdd  #xb4)]
    [`(or ixl)             (zdd  #xb5)]
    [`(or (ix ,(? zoff? n))) (zdds #xb6 n)]
    [`(or iyh)             (zfd  #xb4)]
    [`(or iyl)             (zfd  #xb5)]
    [`(or (iy ,(? zoff? n))) (zfds #xb6 n)]
    [`(or ,(? zb? n))      (zb   #xf6 n)]
    
    [`(cp b)       (db #xb8)]
    [`(cp c)       (db #xb9)]
    [`(cp d)       (db #xba)]
    [`(cp e)       (db #xbb)]
    [`(cp h)       (db #xbc)]
    [`(cp l)       (db #xbd)]
    [`(cp (hl))    (db #xbe)]
    [`(cp a)       (db #xbf)]
    [`(cp ixh)             (zdd  #xbc)]
    [`(cp ixl)             (zdd  #xbd)]
    [`(cp (ix ,(? zoff? n))) (zdds #xbe n)]
    [`(cp iyh)             (zfd  #xbc)]
    [`(cp iyl)             (zfd  #xbd)]
    [`(cp (iy ,(? zoff? n))) (zfds #xbe n)]
    [`(cp ,(? zb? n))      (zb   #xfe n)]
    
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
    
    [`(djnz ,(? zrel? n))  (zrel #x10 n)]
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
    
    [`(ret)                  (db #xc9)]
    [`(ret nz)               (db #xc0)]
    [`(ret z)                (db #xc8)]
    [`(ret nc)               (db #xd0)]
    [`(ret c)                (db #xd8)]
    
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
    
    [`(pop bc)  (db  #xc1)]
    [`(pop de)  (db  #xd1)]
    [`(pop hl)  (db  #xe1)]
    [`(pop af)  (db  #xf1)]
    
    [`(daa) (db #x27)]
    [`(cpl) (db #x2f)]
    [`(scf) (db #x37)]
    [`(ccf) (db #x3f)]
    
    [`(rlca) (db #x07)]
    [`(rrca) (db #x0f)]
    [`(rla)  (db #x17)]
    [`(rra)  (db #x1f)]
    
    [`(rlc b)    (zcb #x00)]
    [`(rlc c)    (zcb #x01)]
    [`(rlc d)    (zcb #x02)]
    [`(rlc e)    (zcb #x03)]
    [`(rlc h)    (zcb #x04)]
    [`(rlc l)    (zcb #x05)]
    [`(rlc (hl)) (zcb #x06)]
    [`(rlc a)    (zcb #x07)]
    
    [`(rrc b)    (zcb #x08)]
    [`(rrc c)    (zcb #x09)]
    [`(rrc d)    (zcb #x0a)]
    [`(rrc e)    (zcb #x0b)]
    [`(rrc h)    (zcb #x0c)]
    [`(rrc l)    (zcb #x0d)]
    [`(rrc (hl)) (zcb #x0e)]
    [`(rrc a)    (zcb #x0f)]
    
    [`(rl b)     (zcb #x10)]
    [`(rl c)     (zcb #x11)]
    [`(rl d)     (zcb #x12)]
    [`(rl e)     (zcb #x13)]
    [`(rl h)     (zcb #x14)]
    [`(rl l)     (zcb #x15)]
    [`(rl (hl))  (zcb #x16)]
    [`(rl a)     (zcb #x17)]
    
    [`(rr b)     (zcb #x18)]
    [`(rr c)     (zcb #x19)]
    [`(rr d)     (zcb #x1a)]
    [`(rr e)     (zcb #x1b)]
    [`(rr h)     (zcb #x1c)]
    [`(rr l)     (zcb #x1d)]
    [`(rr (hl))  (zcb #x1e)]
    [`(rr a)     (zcb #x1f)]
    
    [`(di)         (db #xf3)]
    [`(ei)         (db #xfb)]
    
    [`(daa)        (db #x27)]
    [`(cpl)        (db #x2f)]
    [`(scf)        (db #x37)]
    [`(ccf)        (db #x3f)]
    
    [`(inc bc)     (db  #x03)]
    [`(inc de)     (db  #x13)]
    [`(inc hl)     (db  #x23)]
    [`(inc sp)     (db  #x33)]
    
    [`(dec bc)     (db  #x0B)]
    [`(dec de)     (db  #x1B)]
    [`(dec hl)     (db  #x2B)]
    [`(dec sp)     (db  #x3B)]
    
    [`(rlc b)      (zcb #x00)]
    [`(rlc c)      (zcb #x01)]
    [`(rlc d)      (zcb #x02)]
    [`(rlc e)      (zcb #x03)]
    [`(rlc h)      (zcb #x04)]
    [`(rlc l)      (zcb #x05)]
    [`(rlc (hl))   (zcb #x06)]
    [`(rlc a)      (zcb #x07)]
    
    [`(rrc b)      (zcb #x08)]
    [`(rrc c)      (zcb #x09)]
    [`(rrc d)      (zcb #x0a)]
    [`(rrc e)      (zcb #x0b)]
    [`(rrc h)      (zcb #x0c)]
    [`(rrc l)      (zcb #x0d)]
    [`(rrc (hl))   (zcb #x0e)]
    [`(rrc a)      (zcb #x0f)]
    
    [`(rl b)      (zcb #x10)]
    [`(rl c)      (zcb #x11)]
    [`(rl d)      (zcb #x12)]
    [`(rl e)      (zcb #x13)]
    [`(rl h)      (zcb #x14)]
    [`(rl l)      (zcb #x15)]
    [`(rl (hl))   (zcb #x16)]
    [`(rl a)      (zcb #x17)]
    
    [`(rr b)      (zcb #x18)]
    [`(rr c)      (zcb #x19)]
    [`(rr d)      (zcb #x1a)]
    [`(rr e)      (zcb #x1b)]
    [`(rr h)      (zcb #x1c)]
    [`(rr l)      (zcb #x1d)]
    [`(rr (hl))   (zcb #x1e)]
    [`(rr a)      (zcb #x1f)]
    
    [`(bit ,(? zbit? n) b)    (zcb (+ (* 8 n) #x40))]
    [`(bit ,(? zbit? n) c)    (zcb (+ (* 8 n) #x41))]
    [`(bit ,(? zbit? n) d)    (zcb (+ (* 8 n) #x42))]
    [`(bit ,(? zbit? n) e)    (zcb (+ (* 8 n) #x43))]
    [`(bit ,(? zbit? n) h)    (zcb (+ (* 8 n) #x44))]
    [`(bit ,(? zbit? n) l)    (zcb (+ (* 8 n) #x45))]
    [`(bit ,(? zbit? n) (hl)) (zcb (+ (* 8 n) #x46))]
    [`(bit ,(? zbit? n) a)    (zcb (+ (* 8 n) #x47))]
    
    [`(res ,(? zbit? n) b)    (zcb (+ (* 8 n) #x80))]
    [`(res ,(? zbit? n) c)    (zcb (+ (* 8 n) #x81))]
    [`(res ,(? zbit? n) d)    (zcb (+ (* 8 n) #x82))]
    [`(res ,(? zbit? n) e)    (zcb (+ (* 8 n) #x83))]
    [`(res ,(? zbit? n) h)    (zcb (+ (* 8 n) #x84))]
    [`(res ,(? zbit? n) l)    (zcb (+ (* 8 n) #x85))]
    [`(res ,(? zbit? n) (hl)) (zcb (+ (* 8 n) #x86))]
    [`(res ,(? zbit? n) a)    (zcb (+ (* 8 n) #x87))]
    
    [`(set ,(? zbit? n) b)    (zcb (+ (* 8 n) #xc0))]
    [`(set ,(? zbit? n) c)    (zcb (+ (* 8 n) #xc1))]
    [`(set ,(? zbit? n) d)    (zcb (+ (* 8 n) #xc2))]
    [`(set ,(? zbit? n) e)    (zcb (+ (* 8 n) #xc3))]
    [`(set ,(? zbit? n) h)    (zcb (+ (* 8 n) #xc4))]
    [`(set ,(? zbit? n) l)    (zcb (+ (* 8 n) #xc5))]
    [`(set ,(? zbit? n) (hl)) (zcb (+ (* 8 n) #xc6))]
    [`(set ,(? zbit? n) a)    (zcb (+ (* 8 n) #xc7))]
    
    [`(sla b)    (zcb #x20)]
    [`(sla c)    (zcb #x21)]
    [`(sla d)    (zcb #x22)]
    [`(sla e)    (zcb #x23)]
    [`(sla h)    (zcb #x24)]
    [`(sla l)    (zcb #x25)]
    [`(sla (hl)) (zcb #x26)]
    [`(sla a)    (zcb #x27)]
    
    [`(sra b)    (zcb #x28)]
    [`(sra c)    (zcb #x29)]
    [`(sra d)    (zcb #x2a)]
    [`(sra e)    (zcb #x2b)]
    [`(sra h)    (zcb #x2c)]
    [`(sra l)    (zcb #x2d)]
    [`(sra (hl)) (zcb #x2e)]
    [`(sra a)    (zcb #x2f)]
    
    
    [`(srl b)    (zcb #x38)]
    [`(srl c)    (zcb #x39)]
    [`(srl d)    (zcb #x3a)]
    [`(srl e)    (zcb #x3b)]
    [`(srl h)    (zcb #x3c)]
    [`(srl l)    (zcb #x3d)]
    [`(srl (hl)) (zcb #x3e)]
    [`(srl a)    (zcb #x3f)]
    
    [rest (%asm-base rest)]
    ))

(define %z80-excl
  (match-lambda
    [`(ld b ixh)               (zdd  #x44)]
    [`(ld b ixl)               (zdd  #x45)]
    [`(ld b (ix ,(? zoff? s))) (zdds #x46 s)]
    [`(ld b iyh)               (zfd  #x44)]
    [`(ld b iyl)               (zfd  #x45)]
    [`(ld b (iy ,(? zoff? s))) (zfds #x46 s)]
    
    [`(ld c ixh)               (zdd  #x4c)]
    [`(ld c ixl)               (zdd  #x4d)]
    [`(ld c (ix ,(? zoff? s))) (zdds #x4e s)]
    [`(ld c iyh)               (zfd  #x4c)]
    [`(ld c iyl)               (zfd  #x4d)]
    [`(ld c (iy ,(? zoff? s))) (zfds #x4e s)]
    
    [`(ld d ixh)               (zdd  #x54)]
    [`(ld d ixl)               (zdd  #x55)]
    [`(ld d (ix ,(? zoff? s))) (zdds #x56 s)]
    [`(ld d iyh)               (zfd  #x54)]
    [`(ld d iyl)               (zfd  #x55)]
    [`(ld d (iy ,(? zoff? s))) (zfds #x56 s)]
    
    [`(ld e ixh)               (zdd  #x5c)]
    [`(ld e ixl)               (zdd  #x5d)]
    [`(ld e (ix ,(? zoff? s))) (zdds #x5e s)]
    [`(ld e iyh)               (zfd  #x5c)]
    [`(ld e iyl)               (zfd  #x5d)]
    [`(ld e (iy ,(? zoff? s))) (zfds #x5e s)]
    
    [`(ld h ixh)               (zdd  #x64)]
    [`(ld h ixl)               (zdd  #x65)]
    [`(ld h (ix ,(? zoff? s))) (zdds #x66 s)]
    [`(ld h iyh)               (zfd  #x64)]
    [`(ld h iyl)               (zfd  #x65)]
    [`(ld h (iy ,(? zoff? s))) (zfds #x66 s)]
    
    [`(ld l ixh)               (zdd  #x6c)]
    [`(ld l ixl)               (zdd  #x6d)]
    [`(ld l (ix ,(? zoff? s))) (zdds #x6e s)]
    [`(ld l iyh)               (zfd  #x6c)]
    [`(ld l iyl)               (zfd  #x6d)]
    [`(ld l (iy ,(? zoff? s))) (zfds #x6e s)]
    
    [`(ld ixh b)                 (zdd   #x60)]
    [`(ld ixh c)                 (zdd   #x61)]
    [`(ld ixh d)                 (zdd   #x62)]
    [`(ld ixh e)                 (zdd   #x63)]
    [`(ld ixh h)                 (zdd   #x64)]
    [`(ld ixh l)                 (zdd   #x65)]
    [`(ld ixh (ix ,(? zoff? s))) (zdds #x66 s)]
    [`(ld ixh a)                 (zdd   #x67)]
    [`(ld ixh ,(? zb? n))        (zddb #x26 n)]

    [`(ld iyh b)                 (zfd  #x68)]
    [`(ld iyh c)                 (zfd  #x69)]
    [`(ld iyh d)                 (zfd  #x6a)]
    [`(ld iyh e)                 (zfd  #x6b)]
    [`(ld iyh h)                 (zfd  #x6c)]
    [`(ld iyh l)                 (zfd  #x6d)]
    [`(ld iyh (iy ,(? zoff? s))) (zfds #x6e s)]
    [`(ld iyh a)                 (zfd  #x6f)]
    [`(ld iyh ,(? zb? n))        (zfdb #x26 n)]
    
    [`(ld (ix ,(? zoff? n)) b)             (zdds  #x70 n)]
    [`(ld (ix ,(? zoff? n)) c)             (zdds  #x71 n)]
    [`(ld (ix ,(? zoff? n)) d)             (zdds  #x72 n)]
    [`(ld (ix ,(? zoff? n)) e)             (zdds  #x73 n)]
    [`(ld (ix ,(? zoff? n)) h)             (zdds  #x74 n)]
    [`(ld (ix ,(? zoff? n)) l)             (zdds  #x75 n)]
    [`(ld (ix ,(? zoff? n)) ,(? zb? b))    (zddsb #x76 n b)]
    [`(ld (ix ,(? zoff? n)) a)             (zdds  #x77 n)]

    [`(ld (iy ,(? zoff? n)) b)             (zfds  #x70 n)]
    [`(ld (iy ,(? zoff? n)) c)             (zfds  #x71 n)]
    [`(ld (iy ,(? zoff? n)) d)             (zfds  #x72 n)]
    [`(ld (iy ,(? zoff? n)) e)             (zfds  #x73 n)]
    [`(ld (iy ,(? zoff? n)) h)             (zfds  #x74 n)]
    [`(ld (iy ,(? zoff? n)) l)             (zfds  #x75 n)]
    [`(ld (iy ,(? zoff? n)) ,(? zb? b))    (zfdsb #x76 n b)]
    [`(ld (iy ,(? zoff? n)) a)             (zfds  #x77 n)]
    
    
    [`(ld a ixh)               (zdd  #x7c)]
    [`(ld a ixl)               (zdd  #x7d)]
    [`(ld a (ix ,(? zoff? s))) (zdds #x7e s)]
    [`(ld a iyh)               (zfd  #x7c)]
    [`(ld a iyl)               (zfd  #x7d)]
    [`(ld a (iy ,(? zoff? s))) (zfds #x7e s)]
    
    [`(ld hl (,(? za? w))) (zw #x2a w)]
    [`(ld (,(? za? w)) hl) (zw #x22 w)]
    
    [`(ld ix ,(? zw? w))   (zfdw #x21 w)]
    [`(ld iy ,(? zw? w))   (zfdw #x21 w)]
    
    [`(ld ix (,(? za? w))) (zddw #x2a w)]
    [`(ld iy (,(? za? w))) (zfdw #x2a w)]
    
    [`(ld (,(? za? w)) ix) (zddw #x22 w)]
    [`(ld (,(? za? w)) iy) (zfdw #x22 w)]
    
    [`(ld sp ix)   (zdd #xf9)]
    [`(ld sp iy)   (zfd #xf9)]
    
    [`(add a ixh)             (zdd  #xa4)]
    [`(add a ixl)             (zdd  #xa5)]
    [`(add a (ix ,(? zoff? n))) (zdds #xa6 n)]
    [`(add a iyh)             (zfd  #xa4)]
    [`(add a iyl)             (zfd  #xa5)]
    [`(add a (iy ,(? zoff? n))) (zfds #xa6 n)]
    
    [`(add ix bc)  (zdd #x09)]
    [`(add ix de)  (zdd #x19)]
    [`(add ix hl)  (zdd #x29)]
    [`(add ix sp)  (zdd #x39)]
    [`(add iy bc)  (zfd #x09)]
    [`(add iy de)  (zfd #x19)]
    [`(add iy hl)  (zfd #x29)]
    [`(add iy sp)  (zfd #x39)]
    
    [`(adc a ixh)             (zdd  #x8c)]
    [`(adc a ixl)             (zdd  #x8d)]
    [`(adc a (ix ,(? zoff? n))) (zdds #x8e n)]
    [`(adc a iyh)             (zfd  #x8c)]
    [`(adc a iyl)             (zfd  #x8d)]
    [`(adc a (iy ,(? zoff? n))) (zfds #x8e n)]
    
    [`(sub ixh)      (zdd #x94)]
    [`(sub ixl)      (zdd #x95)]
    [`(sub (ix ,(? zoff? s)))   (zdds #x96 s)]
    [`(sub iyh)      (zfd #x94)]
    [`(sub iyl)      (zfd #x95)]
    [`(sub (iy ,(? zoff? s)))   (zfds #x96 s)]
    
    [`(sbc a ixh)    (zdd #x9c)]
    [`(sbc a ixl)    (zdd #x9d)]
    [`(sbc a (ix ,(? zoff? s))) (zdds #x9e)]
    [`(sbc a iyh)    (zfd #x9c)]
    [`(sbc a iyl)    (zfd #x9d)]
    [`(sbc a (iy ,(? zoff? s))) (zfds #x9e)]
    
    [`(and ixh)             (zdd  #xa4)]
    [`(and ixl)             (zdd  #xa5)]
    [`(and (ix ,(? zoff? n))) (zdds #xa6 n)]
    [`(and iyh)             (zfd  #xa4)]
    [`(and iyl)             (zfd  #xa5)]
    [`(and (iy ,(? zoff? n))) (zfds #xa6 n)]
    
    [`(xor ixh)             (zdd  #xac)]
    [`(xor ixl)             (zdd  #xad)]
    [`(xor (ix ,(? zoff? n))) (zdds #xae n)]
    [`(xor iyh)             (zfd  #xac)]
    [`(xor iyl)             (zfd  #xad)]
    [`(xor (iy ,(? zoff? n))) (zfds #xae n)]
    
    [`(or ixh)             (zdd  #xb4)]
    [`(or ixl)             (zdd  #xb5)]
    [`(or (ix ,(? zoff? n))) (zdds #xb6 n)]
    [`(or iyh)             (zfd  #xb4)]
    [`(or iyl)             (zfd  #xb5)]
    [`(or (iy ,(? zoff? n))) (zfds #xb6 n)]
    
    [`(cp ixh)             (zdd  #xbc)]
    [`(cp ixl)             (zdd  #xbd)]
    [`(cp (ix ,(? zoff? n))) (zdds #xbe n)]
    [`(cp iyh)             (zfd  #xbc)]
    [`(cp iyl)             (zfd  #xbd)]
    [`(cp (iy ,(? zoff? n))) (zfds #xbe n)]
    
    [`(exx)        (db #xd9)]
    
    [`(ex af af*)                     (db #x08)]
    
    [`(jp (ix))          (zdd #xe9)]
    [`(jp (iy))          (zfd #xe9)]
    
    [`(djnz ,(? zrel? n))  (zrel #x10 n)]
    
    [`(call po ,(? za? n)) (zw #xe4 n)]
    [`(call pe ,(? za? n)) (zw #xec n)]
    [`(call p ,(? za? n))  (zw #xf4 n)]
    [`(call m ,(? za? n))  (zw #xfc n)]
    
    [`(ret po)               (db #xe0)]
    [`(ret pe)               (db #xe8)]
    [`(ret p)                (db #xf0)]
    [`(ret p)                (db #xf8)]
    
    [`(reti) (zed #x4d)]
    [`(retn) (zed #x45)]
    
    
    [`(push ix)  (zdd #xe5)]
    [`(push iy)  (zfd #xe5)]
    
    [`(pop ix)  (zdd #xe1)]
    [`(pop iy)  (zfd #xe1)]
    
    [`(in a (,(? zb? n))) (zb #xdb n)]
    [`(in a (c))          (zed #x78)]
    [`(in b (c))          (zed #x40)]
    [`(in c (c))          (zed #x48)]
    [`(in d (c))          (zed #x50)]
    [`(in e (c))          (zed #x58)]
    [`(in h (c))          (zed #x60)]
    [`(in l (c))          (zed #x68)]
    [`(ini)               (zed #xa2)]
    [`(inir)              (zed #xb2)]
    [`(ind)               (zed #xaa)]
    [`(indr)              (zed #xba)]

    [`(out (,(? zb? n)) a) (zb #xd3 n)]
    [`(out (c) a)          (zed #x79)]
    [`(out (c) b)          (zed #x41)]
    [`(out (c) c)          (zed #x49)]
    [`(out (c) d)          (zed #x51)]
    [`(out (c) e)          (zed #x59)]
    [`(out (c) h)          (zed #x61)]
    [`(out (c) l)          (zed #x69)]
    [`(outi)               (zed #xa3)]
    [`(outir)              (zed #xb3)]
    [`(outd)               (zed #xab)]
    [`(outdr)              (zed #xbb)]
    
    [`(ldi)                (zed #xa0)]
    [`(ldir)               (zed #xb0)]
    [`(ldd)                (zed #xa8)]
    [`(lddr)               (zed #xb8)]
    [`(cpi)                (zed #xa1)]
    [`(cpir)               (zed #xb1)]
    [`(cpd)                (zed #xa9)]
    [`(cpdr)               (zed #xb9)]
    
    [`(neg) (zed #x44)]
    
    [`(rld)  (zed #x6f)]
    [`(rrd)  (zed #x67)]
    
    [`(rlc b)    (zcb #x00)]
    [`(rlc c)    (zcb #x01)]
    [`(rlc d)    (zcb #x02)]
    [`(rlc e)    (zcb #x03)]
    [`(rlc h)    (zcb #x04)]
    [`(rlc l)    (zcb #x05)]
    [`(rlc (hl)) (zcb #x06)]
    [`(rlc a)    (zcb #x07)]
    
    [`(rrc b)    (zcb #x08)]
    [`(rrc c)    (zcb #x09)]
    [`(rrc d)    (zcb #x0a)]
    [`(rrc e)    (zcb #x0b)]
    [`(rrc h)    (zcb #x0c)]
    [`(rrc l)    (zcb #x0d)]
    [`(rrc (hl)) (zcb #x0e)]
    [`(rrc a)    (zcb #x0f)]
    
    [`(rl b)     (zcb #x10)]
    [`(rl c)     (zcb #x11)]
    [`(rl d)     (zcb #x12)]
    [`(rl e)     (zcb #x13)]
    [`(rl h)     (zcb #x14)]
    [`(rl l)     (zcb #x15)]
    [`(rl (hl))  (zcb #x16)]
    [`(rl a)     (zcb #x17)]
    
    [`(rr b)     (zcb #x18)]
    [`(rr c)     (zcb #x19)]
    [`(rr d)     (zcb #x1a)]
    [`(rr e)     (zcb #x1b)]
    [`(rr h)     (zcb #x1c)]
    [`(rr l)     (zcb #x1d)]
    [`(rr (hl))  (zcb #x1e)]
    [`(rr a)     (zcb #x1f)]
    
    [`(ex af af!) (db #x08)]
    [`(ex (sp) hl) (db #xe3)]
    [`(ex (sp) ix) (zdd #xe3)]
    [`(ex (sp) iy) (zfd #xe3)]
    [`(ex de hl)   (db #xeb)]
    [`(exx)        (db #xd9)]
    
    [`(im 0)       (zed #x46)]
    [`(im 1)       (zed #x56)]
    [`(im 2)       (zed #x5e)]
    [`(ld a i)     (zed #x57)]
    [`(ld i a)     (zed #x47)]
    [`(ld a r)     (zed #x5f)]
    [`(ld r a)     (zed #x4f)]
    
    [`(inc ix)     (zdd #x23)]
    [`(inc iy)     (zfd #x23)]
    
    [`(dec ix)     (zdd #x2B)]
    [`(dec iy)     (zfd #x2B)]
    
    [`(rld)        (zed #x6f)]
    [`(rrd)        (zed #x67)]
    
    [`(rlc (ix ,(? zoff? n)))   (zddcbs #x06 n)]
    [`(rlc (iy ,(? zoff? n)))   (zfdcbs #x06 n)]
    
    [`(rrc (ix ,(? zoff? n)))   (zddcbs #x0e n)]
    [`(rrc (iy ,(? zoff? n)))   (zfdcbs #x0e n)]

    [`(rl (ix ,(? zoff? n)))   (zddcbs #x16 n)]
    [`(rl (iy ,(? zoff? n)))   (zfdcbs #x16 n)]
    
    [`(rr (ix ,(? zoff? n)))   (zddcbs #x1e n)]
    [`(rr (iy ,(? zoff? n)))   (zfdcbs #x1e n)]
    
    [`(bit ,(? zbit? n) (ix ,(? zoff? i))) (zddcbs (+ (* 8 n) #x46) i)]
    [`(bit ,(? zbit? n) (iy ,(? zoff? i))) (zfdcbs (+ (* 8 n) #x46) i)]
    
    [`(res ,(? zbit? n) (ix ,(? zoff? i))) (zddcbs (+ (* 8 n) #x86) i)]
    [`(res ,(? zbit? n) (iy ,(? zoff? i))) (zfdcbs (+ (* 8 n) #x86) i)]
    
    [`(set ,(? zbit? n) (ix ,(? zoff? i))) (zddcbs (+ (* 8 n) #xc6) i)]
    [`(set ,(? zbit? n) (iy ,(? zoff? i))) (zfdcbs (+ (* 8 n) #xc6) i)]
    
    [`(sla (ix ,(? zoff? n))) (zddcbs #x26 n)]
    [`(sla (iy ,(? zoff? n))) (zfdcbs #x26 n)]
    
    [`(sra (ix ,(? zoff? n))) (zddcbs #x2e n)]
    [`(sra (iy ,(? zoff? n))) (zfdcbs #x2e n)]

    [`(sll b)    (zcb #x30)]
    [`(sll c)    (zcb #x31)]
    [`(sll d)    (zcb #x32)]
    [`(sll e)    (zcb #x33)]
    [`(sll h)    (zcb #x34)]
    [`(sll l)    (zcb #x35)]
    [`(sll (hl)) (zcb #x36)]
    [`(sll a)    (zcb #x37)]
    [`(sll (ix ,(? zoff? n))) (zddcbs #x36 n)]
    [`(sll (iy ,(? zoff? n))) (zfdcbs #x36 n)]

    [`(srl (ix ,(? zoff? n))) (zddcbs #x3e n)]
    [`(srl (iy ,(? zoff? n))) (zfdcbs #x3e n)]
    
    [rest (%z80-common rest)]
    ))

(define %z80-gb
  (match-lambda
    [`(add sp ,(? zb? n)) (zb #xe8 n)]
    [`(ld a (+ #xff00 c)) (db #xf2)]
    [`(ld a (+ #xff00 ,(? zb? n))) (zb #xf0 n)]
    [`(ld (+ #xff00 c) a) (db #xe2)]
    [`(ld (+ #xff00 ,(? zb? n)) a) (zb #xe0 n)]
    
    [`(ld a (hl--))       (db #x3a)]
    [`(ld a (hld))        (db #x3a)]
    [`(ldd a (hl))        (db #x3a)]
    
    [`(ld a (hl++))       (db #x2a)]
    [`(ld a (hli))        (db #x2a)]
    [`(ldi a (hl))        (db #x2a)]
    
    [`(ld (hl--) a)       (db #x32)]
    [`(ld (hld) a)        (db #x32)]
    [`(ldd (hl) a)        (db #x32)]
    
    [`(ld (hl++) a)       (db #x22)]
    [`(ld (hli) a)        (db #x22)]
    [`(ldi (hl) a)        (db #x22)]
    
    [`(stop)              (db #x10)]
    
    [`(swap b)            (zcb #x30)]
    [`(swap c)            (zcb #x31)]
    [`(swap d)            (zcb #x32)]
    [`(swap e)            (zcb #x33)]
    [`(swap h)            (zcb #x34)]
    [`(swap l)            (zcb #x35)]
    [`(swap (hl))         (zcb #x36)]
    [`(swap a)            (zcb #x37)]
    
    [`(ldh a ,(? za? n))  (zw #xf0 n)]
    
    [rest (%z80-common rest)]
    ))

(make-architecture 'z80 #f %z80-excl)
(make-architecture 'gbz80 #f %z80-gb)
