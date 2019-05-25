#lang racket

(require "incrementor.rkt")

(struct V (x) #:transparent)
(struct V1 () #:transparent)
(struct V2 () #:transparent)
(struct R (x y) #:transparent)
(struct T (x y) #:transparent)
(struct TC (x y) #:transparent)

(define r1 (list (V 'x)     (R 'x 'y)))
(define r2 (list (V 'y)     (R 'x 'y)))
(define r3 (list (T 'x 'y)  (R 'x 'y)))
(define r4 (list (T 'x 'y)  (T 'x 'z) (R 'z 'y)))
(define r5 (list (TC 'x 'y) (V 'x) (V 'y) (¬ (T 'x 'y))))
(define r6 (list (V1)       (V 'x)))
(define r7 (list (V2)       (V1)))
(define r8 (list (V 'x)     (V2)))

(define P (set r1 r2 r3 r4 r5 r6 r7 r8))

(define G-pred (precedence-lgraph P))

(define G-dep (ltranspose G-pred))

(printf "lscc ~v\n\n" (lscc G-pred))
