#lang racket

(require "datalog.rkt")
(require "naive.rkt")
(require "semi-naive.rkt")
(require "incremental.rkt")

(define r1 (:- #(Reachable x y)    #(Link x y)))
(define r2 (:- #(Reachable x y)    #(Link x z) #(Reachable z y)))

(define P (set r1 r2))
(define E (set #(Link 'a 'b) #(Link 'b 'c) #(Link 'c 'c) #(Link 'c 'd)))

(printf "\nsolve ~a\n\n" (solve-naive P E))
(printf "\nsolve sn ~a\n\n" (solve-semi-naive P E))
(printf "\nsolve i ~a\n\n" (solve-semi-naive-i P E))