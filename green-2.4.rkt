#lang racket

(require "datalog.rkt")
(require "naive.rkt")
(require "semi-naive.rkt")
(require "incremental.rkt")
(require "test.rkt")

(define r1 (:- #(Reachable x y)    #(Link x y)))
(define r2 (:- #(Reachable x y)    #(Link x z) #(Reachable z y)))

(define P (set r1 r2))
(define E (set #(Link 'a 'b) #(Link 'b 'c) #(Link 'c 'c) #(Link 'c 'd)))
(define deltas (list
  (add-tuple #(Link 'd 'e))
  (add-tuple #(Link 'e 'f))
  (add-tuple #(Link 'f 'g))
  (add-tuple #(Link 'g 'h))
  (add-tuple #(Link 'h 'i))
  (add-tuple #(Link 'm 'n))
  (add-tuple #(Link 'l 'm))
  (add-tuple #(Link 'k 'l))
  (add-tuple #(Link 'j 'k))
  (add-tuple #(Link 'i 'j))
))

(correctness-test P E (list solve-naive solve-semi-naive) deltas)
(performance-test P E (list solve-naive solve-semi-naive solve-semi-naive-i) deltas)
