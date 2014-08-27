#lang racket

(require "plasm.rkt")

(define (z80b? n)
  (number? n))

(define (z80b op n)
  (db op n))

(define (z80w? n)
  (number? n))

(define (z80w op n)
  (db op (asm-b 0 n) (asm-b 1 n)))

(define (z80rel? n)
  (number? n))

(define (z80rel op n)
  (db op (->@ n)))

(architecture
 'z80 #f
  (match-lambda
    [`(nop)       (db #x00)]
    
    [`(ld b b)    (db #x40)]
    [`(ld b c)    (db #x41)]
    [`(ld b d)    (db #x42)]
    [`(ld b e)    (db #x43)]
    [`(ld b h)    (db #x44)]
    [`(ld b l)    (db #x45)]
    [`(ld b (hl)) (db #x46)]
    [`(ld b a)    (db #x47)]
    
    [`(ld c b)    (db #x48)]
    [`(ld c c)    (db #x49)]
    [`(ld c d)    (db #x4a)]
    [`(ld c e)    (db #x4b)]
    [`(ld c h)    (db #x4c)]
    [`(ld c l)    (db #x4d)]
    [`(ld c (hl)) (db #x4e)]
    [`(ld c a)    (db #x4f)]

    [`(ld d b)    (db #x50)]
    [`(ld d c)    (db #x51)]
    [`(ld d d)    (db #x52)]
    [`(ld d e)    (db #x53)]
    [`(ld d h)    (db #x54)]
    [`(ld d l)    (db #x55)]
    [`(ld d (hl)) (db #x56)]
    [`(ld d a)    (db #x57)]
    
    [`(ld e b)    (db #x58)]
    [`(ld e c)    (db #x59)]
    [`(ld e d)    (db #x5a)]
    [`(ld e e)    (db #x5b)]
    [`(ld e h)    (db #x5c)]
    [`(ld e l)    (db #x5d)]
    [`(ld e (hl)) (db #x5e)]
    [`(ld e a)    (db #x5f)]

    [`(ld h b)    (db #x60)]
    [`(ld h c)    (db #x61)]
    [`(ld h d)    (db #x62)]
    [`(ld h e)    (db #x63)]
    [`(ld h h)    (db #x64)]
    [`(ld h l)    (db #x65)]
    [`(ld h (hl)) (db #x66)]
    [`(ld h a)    (db #x67)]
    
    [`(ld l b)    (db #x68)]
    [`(ld l c)    (db #x69)]
    [`(ld l d)    (db #x6a)]
    [`(ld l e)    (db #x6b)]
    [`(ld l h)    (db #x6c)]
    [`(ld l l)    (db #x6d)]
    [`(ld l (hl)) (db #x6e)]
    [`(ld l a)    (db #x6f)]

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
    [`(ld a a)    (db #x7f)]

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

    [`(ld bc ,(? z80w? n))            (z80w #x01 n)]
    [`(ld (bc) a)                     (db #x02)]
    [`(inc bc)                        (db #x03)]
    [`(inc b)                         (db #x04)]
    [`(dec b)                         (db #x05)]
    [`(ld b ,(? z80b? n))             (z80b #x06 n)]
    [`(rlca)                          (db #x07)]
    [`(ex af af*)                     (db #x08)]
    [`(add hl bc)                     (db #x09)]
    [`(ld a (bc))                     (db #x0a)]
    [`(dec bc)                        (db #x0b)]
    [`(inc c)                         (db #x0c)]
    [`(dec c)                         (db #x0d)]
    [`(ld c ,(? z80b? n))             (z80b #x0e n)]
    [`(rrca)                          (db #x0f)]
    
    [`(djnz ,(? z80rel? n))           (z80rel #x10 n)]
    [`(ld de ,(? z80w? n))            (z80w #x11 n)]
    [`(ld (de) a)                     (db #x12)]
    [`(inc de)                        (db #x13)]
    [`(inc d)                         (db #x14)]
    [`(dec d)                         (db #x15)]
    [`(ld d ,(? z80b? n))             (z80b #x16 n)]
    
    ))