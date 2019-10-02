#lang racket

(require "incrementor.rkt")

(define r1 (:- #(V x)     #(R x y)))
(define r2 (:- #(V y)     #(R x y)))
(define r3 (:- #(T x y)   #(R x y)))
(define r4 (:- #(T x y)   #(T x z) #(R z y)))
(define r5 (:- #(TC x y)  #(V x) #(V y) (¬ #(T x y))))

(define P (set r1 r2 r3 r4 r5))

(printf "stratify ~v\n\n" (stratify P))

