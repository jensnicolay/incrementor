#lang racket

(require "incrementor.rkt")

(define r1 (list #(V x)     #(R x y)))
(define r2 (list #(V y)     #(R x y)))
(define r3 (list #(T x y)   #(R x y)))
(define r4 (list #(T x y)   #(T x z) #(R z y)))
(define r5 (list #(TC x y)  #(V x) #(V y) (¬ #(T x y))))

(define P (set r1 r2 r3 r4 r5))

(printf "stratify ~v\n\n" (stratify P))

