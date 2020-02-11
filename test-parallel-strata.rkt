#lang racket

(require "datalog.rkt")
(require "naive.rkt")
(require "semi-naive.rkt")
(require "incremental.rkt")
(require "test.rkt")

(define r1 (:- #(Reachable x y)   #(Link x y)))
(define r2 (:- #(Reachable x y)   #(Link x z) #(Reachable z y)))
(define r3 (:- #(Node x)          #(Link x y)))
(define r4 (:- #(Node y)          #(Link x y)))
(define r5 (:- #(Unreachable x y) #(Node x) #(Node y) (¬ #(Reachable x y))))


(define P (set r1 r2 r3 r4 r5))
(define E (set #(Link 1 2) #(Link 2 3)))

(define sr1 (solve-incremental P E))

(define sr2 ((solver-result-delta-solver sr1) (list (add-tuple #(Link 2 1)))))
(define tuples (solver-result-tuples sr2))
(printf "2 ~a\n" (sort-tuples tuples))
