
#lang racket

(require "datalog.rkt")
(require "naive.rkt")
;(require "semi-naive.rkt")
;(require "incremental.rkt")

(define tests 0)
(define errors 0)

(define (report-error solver P E I S)
  (set! errors (add1 errors))
  (printf "error ~a ~a\nP ~a\nI ~a\nS ~a\n\n" solver tests P (sort-tuples (set-union E I)) (sort-tuples S)))

(define (test P I)
  (set! tests (add1 tests))
  (define E (set #(I 123)))
  (define expected (set-union E I))
  (define Sn (solver-result-tuples (solve-naive P E)))
  ;(define Ssn (solver-result-tuples (solve-semi-naive P E)))
  ;(define Si (solver-result-tuples (solve-incremental P E)))

  (unless (equal? expected Sn)
    (report-error 'naive P E I Sn))

  ; (unless (equal? expected Ssn)
  ;   (report-error 'semi-naive P E I Ssn))

  ; (unless (equal? expected Si)
  ;   (report-error 'incremental P E I Si))
)


(test (set (:- #(R y)   #(I y) #(= y 123)))
      (set #(R 123)))

(test (set (:- #(R y)   #(I y) #(= 123 y)))
      (set #(R 123)))

(test (set (:- #(R x)   #(I y) #(= x (+ y 2))))
      (set #(R 125)))

(test (set (:- #(R x)   #(I y) #(= x `#(T ,(+ y 2)))))
      (set #(R #(T 125))))

(test (set (:- #(R x)   #(I y) #(= #(T x) `#(T ,(+ y 2)))))
      (set #(R 125)))


(printf "\n\nERRORS: ~a\n\n" errors)  
