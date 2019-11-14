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

(correctness-test P E (list solve-naive solve-semi-naive solve-semi-naive-i)
  (add-tuple #(Link 'd 'e))
  (add-tuple #(Link 'e 'f))
  (add-tuple #(Link 'f 'g))
  (add-tuple #(Link 'g 'h))
  (add-tuple #(Link 'h 'i))
  (add-tuple #(Link 'i 'j))  
  )
  