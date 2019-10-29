#lang racket

(require "datalog.rkt")
(require "naive.rkt")
(require "semi-naive.rkt")

(define r1 (:- #(Reachable x y)   #(Link x y)))
(define r2 (:- #(Reachable x y)   #(Link x z) #(Reachable z y)))
(define r3 (:- #(Node x)          #(Link x y)))
(define r4 (:- #(Node y)          #(Link x y)))
(define r5 (:- #(Unreachable x y) #(Node x) #(Node y) (¬ #(Reachable x y))))

(define P (set r1 r2 r3 r4 r5))
(define E (set #(Link 'a 'b) #(Link 'b 'c) #(Link 'c 'c) #(Link 'c 'd)))

;(printf "stratify ~v\n\n" (stratify P))

;(printf "~v\n" (fire r1 E))


(printf "\nsolve ~a\n\n" (solve-naive P E))
