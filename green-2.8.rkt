#lang racket

(require "incrementor.rkt")

(define r1 (list #(Reachable x y)   #(Link x y)))
(define r2 (list #(Reachable x y)   #(Link x z) #(Reachable z y)))
(define r3 (list #(Node x)          #(Link x y)))
(define r4 (list #(Node y)          #(Link x y)))
(define r5 (list #(Unreachable x y) #(Node x) #(Node y) (¬ #(Reachable x y))))

(define P (set r1 r2 r3 r4 r5))
(define E (set #(Link 'a 'b) #(Link 'b 'c) #(Link 'c 'c) #(Link 'c 'd)))

;(printf "stratify ~v\n\n" (stratify P))

;(printf "~v\n" (fire r1 E))

(printf "\nsolve ~a\n\n" (solve-semi-naive P E)) 