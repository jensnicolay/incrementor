#lang racket

(require "datalog.rkt")
(require "incremental.rkt")
(require "test.rkt")

(define r1 (:- #(Reachable x y)    #(Link x y)))
(define r2 (:- #(Reachable x y)    #(Link x z) #(Reachable z y)))

(define P (set r1 r2))
(define E (set #(Link 'a 'b) #(Link 'b 'c) #(Link 'c 'c) #(Link 'c 'd)))

(perform-test P E)
  ;(add-tuple #(Link 'd 'e)))
