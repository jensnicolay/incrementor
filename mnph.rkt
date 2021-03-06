#lang racket

(require "incrementor.rkt")


(define r1 (:- #(T x y) #(R x y) (¬ #(A x))))
(define r2 (:- #(T x y) #(S x y) #(A x)))
(define r3 (:- #(B y) #(T x y) #(B x)))

(define P (set r1 r2 r3))
(define E (set #(R 'b 'e) #(S 'b 'f) #(B 'a) 
                #(T 'a 'b) #(T 'b 'c) #(T 'c 'd) 
                #(T 'd 'c) #(T 'e 'c) #(T 'f 'g) #(T 'g 'c)))
