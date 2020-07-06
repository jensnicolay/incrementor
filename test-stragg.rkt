
#lang racket

(require "datalog.rkt")
(require "naive.rkt")
(require "semi-naive.rkt")
; (require "incremental.rkt")

(define tests 0)
(define errors 0)

(define (report-error solver P E I S)
  (set! errors (add1 errors))
  (printf "error ~a ~a\nP ~a\nE ~a\nI ~a\nS ~a\n\n" solver tests P E (sort-tuples (set-union E I)) (sort-tuples S)))

(define (test P E I)
  (set! tests (add1 tests))
  (define expected (set-union E I))
  (define Sn (solver-result-tuples (solve-naive P E)))
  (define Ssn (solver-result-tuples (solve-semi-naive P E)))
;   (define Si (solver-result-tuples (solve-incremental P E)))

  (unless (equal? expected Sn)
    (report-error 'naive P E I Sn))

  (unless (equal? expected Ssn)
    (report-error 'semi-naive P E I Ssn))

;   (unless (equal? expected Si)
;     (report-error 'incremental P E I Si))
)

(test (set (:- #(R g #:sum x) #(I g x)))
      (set #(I a 1) #(I a 2) #(I b 33))
      (set #(R a 3) #(R b 33)))

(test (set (:- #(R g #:count x) #(I g x)))
      (set #(I a 1) #(I a 2) #(I b 33))
      (set #(R a 2) #(R b 1)))

(test (set (:- #(R g #:min x) #(I g x)))
      (set #(I a 1) #(I a 2) #(I b 33))
      (set #(R a 1) #(R b 33)))

(test (set (:- #(R g #:max x) #(I g x)))
      (set #(I a 1) #(I a 2) #(I b 33))
      (set #(R a 2) #(R b 33)))


(printf "\n\nERRORS: ~a\n\n" errors)  
