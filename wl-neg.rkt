#lang racket

(require "datalog.rkt")
(require "naive.rkt")
(require "semi-naive.rkt")
(require "incremental.rkt")
(require "wl-test.rkt")

(define r1 (:- #(F) (¬ #(T))))

(define P (set r1))
(define E (set #(T)))

(define deltas (list
  (remove-tuple #(T))
  (add-tuple #(T))
))

; (correctness-test P E (list solve-naive solve-semi-naive) deltas)
; (performance-test P E (list solve-naive solve-semi-naive) deltas)
(correctness-test P E (list solve-naive solve-semi-naive solve-incremental) deltas)
(performance-test P E (list solve-naive solve-semi-naive solve-incremental) deltas)
