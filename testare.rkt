#lang racket

(require "datalog.rkt")
(require "naive.rkt")
(require "semi-naive.rkt")

(define P (set
  
  (#(R1 d) . :- . #(X l) #(= d1 ,(list-ref l 0)) #(= d2 ,(list-ref l 1)) #(= d ,(+ d1 d2)))
  
  (#(R2 d) . :- . #(X l) #(%select d1 0 l) #(%select d2 1 l) #(= d ,(+ d1 d2)))

  (#(R3 'ok) . :- . #(X3 d) (¬ #(= d #f)))

  (#(R4 'ok) . :- . #(X4 #f))
  (#(R4b 'ok) . :- . #(X4 d) #(= d #f))

  (#(R5) . :- . #(X4 d) (¬ #(= d #f)))

  (#(R6 'ok) . :- . #(= ,(* 2 2) ,(+ 2 2)))
  (#(R6b 'ok) . :- . #(= ,(* 3 3) ,(+ 3 3)))

  (#(R7 d) . :- . #(X l) #(%index-of 3 d l))
  ;(#(R7b d) . :- . #(X l) #(%index-of ,(+ 1 2) d l)) ; qq not supported in that pos
  
))

(define E (set #(X (5 3)) #(X3 123) #(X4 #f)))
(define S (solver-result-tuples (solve-semi-naive P E)))

(printf "\nsolve ~a\n\n" S)

(define (test facts name-R d)
  (let ((Rs (sequence->list (sequence-filter (lambda (a) (eq? name-R (atom-name a))) (in-set facts)))))
    (if (= (length Rs) 1)
        (let ((actual (vector-ref (car Rs) 1)))
          (unless (equal? d actual)
            (error 'test "wrong result for ~a: expected ~a, got ~a" name-R d actual)))
        (error 'test "wrong number of results for ~a: ~a" name-R Rs))))

(define (test-fail facts name-R)
  (let ((Rs (sequence->list (sequence-filter (lambda (a) (eq? name-R (atom-name a))) (in-set facts)))))
    (unless (zero? (length Rs))
      (error 'test "wrong result for ~a: expected failure, got ~a" name-R Rs))))


(test S 'R1 8)
(test S 'R2 8)
(test S 'R3 'ok)
(test S 'R4 'ok)
(test S 'R4b 'ok)
(test-fail S 'R5)
(test S 'R6 'ok)
(test-fail S 'R6b)
(test S 'R7 1)
;(test S 'R7b 1)