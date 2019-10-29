#lang racket

(require "datalog.rkt")
(require "naive.rkt")
(require "semi-naive.rkt")
(require "incremental.rkt")

(provide perform-test)

(define (perform-test P E0 deltas)


  (match-define (solver-result tuples-naive duration-naive0 num-derived-naive) (solve-naive P E0))
  (match-define (solver-result tuples-semi-naive duration-semi-naive0 num-derived-semi-naive) (solve-semi-naive P E0))
  (match-define (solver-result-i incremental-solver0 tuples-semi-naive-i duration-semi-naive-i0 num-derived-semi-naive-i) (solve-semi-naive-i P E0))


  (unless (equal? tuples-naive tuples-semi-naive)
    (error "semi-naive tuples do not match naive tuples"))

  (unless (<= num-derived-semi-naive num-derived-naive)
    (error "more tuples derived in semi-naive than naive"))

  (when (= num-derived-semi-naive num-derived-naive)
    (printf "same number of tuples derived in semi-naive as in naive\n"))

  (when (>= duration-semi-naive0 duration-naive0)
    (printf "semi-naive takes as long or longer as naive: ~a ~a\n" duration-semi-naive0 duration-naive0))


  (unless (equal? tuples-naive tuples-semi-naive-i)
    (error "semi-naive-i tuples do not match naive tuples"))

  (unless (<= num-derived-semi-naive-i num-derived-naive)
    (error "more tuples derived in semi-naive-i than naive"))

  (when (= num-derived-semi-naive-i num-derived-naive)
    (printf "same number of tuples derived in semi-naive-i as in naive\n"))

  (when (>= duration-semi-naive-i0 duration-naive0)
    (printf "initial semi-naive-i takes as long or longer as naive: ~a ~a\n" duration-semi-naive-i0 duration-naive0))

  ; TODO? test whether semi-naive-i takes 'much' longer than semi-naive (e.g., x2)


  (let delta-loop ((deltas deltas) (deltas-acc (set)) (E-acc E0) (incremental-solver-acc incremental-solver0))
  
    (if (null? deltas)
        'done
        (let* ((delta (car deltas))
                (deltas-acc* (set-add deltas-acc delta))
                (E-acc* (apply-deltas (set delta) E-acc)))
          (match-let (((solver-result tuples-naive duration-naive num-derived-naive) (solve-naive P E-acc*))
                      ((solver-result tuples-semi-naive duration-semi-naive num-derived-semi-naive) (solve-semi-naive P E-acc*))
                      ((solver-result-i incremental-solver-acc* tuples-semi-naive-i duration-semi-naive-i num-derived-semi-naive-i) (incremental-solver-acc (set delta)))
                      ((solver-result-i _ tuples-semi-naive-i duration-semi-naive-i num-derived-semi-naive-i) (incremental-solver0 deltas-acc*)))
            (display "jeuj")
            (delta-loop (cdr deltas) deltas-acc* E-acc* incremental-solver-acc*))))))



