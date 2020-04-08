#lang racket

(require "datalog.rkt")
(require "naive.rkt")
(require "semi-naive.rkt")
(require "incremental.rkt")
(require "wl-test.rkt")

(define r1 (:- #(Reachable x y)    #(Link x y)))
(define r2 (:- #(Reachable x y)    #(Link x z) #(Reachable z y)))

; ADD WORKLOAD

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
  (add-tuple #(Link 'o 'p))
  (add-tuple #(Link 'q 'r))
  (add-tuple #(Link 'p 'q))
  (add-tuple #(Link 's 'u))
  (add-tuple #(Link 's 't))
  (add-tuple #(Link 't 'u))
  (add-tuple #(Link 'v 'w))
  (add-tuple #(Link 'v 'x))
  (add-tuple #(Link 'w 'x))
  (add-tuple #(Link 'y 'y))
  (add-tuple #(Link 'y 'x))
  (add-tuple #(Link 'y 'z))
  (add-tuple #(Link 'z 'a))
))

(correctness-test P E (list solve-naive solve-semi-naive solve-incremental) deltas)
(performance-test P E (list solve-naive solve-semi-naive solve-incremental) deltas)
