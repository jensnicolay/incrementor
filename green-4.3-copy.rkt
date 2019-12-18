#lang racket

(require "datalog.rkt")
(require "naive.rkt")
(require "semi-naive.rkt")
(require "incremental.rkt")
(require "test.rkt")

(define r1 (:- #(Reachable x y)    #(Link x y)))
(define r2 (:- #(Reachable x y)    #(Link x z) #(Reachable z y)))

(define P (set r1 r2))
(define E (set #(Link 'a 'b) #(Link 'b 'c) #(Link 'c 'a) #(Link 'c 'b)))

;(correctness-test P E (list solve-naive solve-semi-naive solve-incremental) deltas)
;(performance-test P E (list solve-naive solve-semi-naive solve-incremental) deltas)

(define sr1 (solve-incremental P E))

(define sr2 ((solver-result-delta-solver sr1) (list (remove-tuple #(Link 'c 'b)))))
(define tuples (solver-result-tuples sr2))
(printf "2 ~a\n" (sort-tuples tuples))

(define sr3 ((solver-result-delta-solver sr2) (list (remove-tuple #(Link 'b 'c)))))
(set! tuples (solver-result-tuples sr3))
(printf "3 ~a\n" (sort-tuples tuples))

(define sr4 ((solver-result-delta-solver sr3) (list (remove-tuple #(Link 'c 'a)))))
(set! tuples (solver-result-tuples sr4))
(printf "4 ~a\n" (sort-tuples tuples))

(define sr5 ((solver-result-delta-solver sr4) (list (remove-tuple #(Link 'a 'b)))))
(set! tuples (solver-result-tuples sr5))
(printf "5 ~a\n" (sort-tuples tuples))