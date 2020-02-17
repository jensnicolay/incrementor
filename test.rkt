#lang racket

(require "datalog.rkt")

(provide correctness-test performance-test)

(define (create-timer)
  (let ((start (current-milliseconds)))
    (lambda ()
      (- (current-milliseconds) start))))

(define (correctness-test P E0 solvers deltas)

  (define (check-equal-tuples*  rs)
    (for ((r2 (in-list (cdr rs))))
      (check-equal-tuples (car rs) r2)))

  (define (check-equal-tuples r1 r2)
    (unless (equal? (solver-result-tuples r1) (solver-result-tuples r2))
      (error 'check-equal-tuples "tuples not equal\n~a\n~a\n" (sort-tuples (solver-result-tuples r1)) (sort-tuples (solver-result-tuples r2)))))

  (define (check-lesseq-derivations* rs)
    (for ((r2 (in-list (cdr rs))))
      (check-lesseq-derivations (car rs) r2)))

  (define (check-lesseq-derivations r1 r2)
    (unless (>= (solver-result-num-derived-tuples r1) (solver-result-num-derived-tuples r2))
      (error 'check-lesseq-derivations "num derived tuples not <=: ~a ~a" (solver-result-num-derived-tuples r1) (solver-result-num-derived-tuples r2))))

  ;;;;;;;;;;;;;;;;

  (printf "=========initial run\n")

  (define initial-solver-results
    (map (lambda (solver)
            (solver P E0))
          solvers))
        
  (check-equal-tuples* initial-solver-results)
  (check-lesseq-derivations* initial-solver-results)

  (define delta-solvers0
    (map solver-result-delta-solver initial-solver-results))

  (let delta-loop ((deltas deltas) (deltas-acc '()) (delta-solvers delta-solvers0))
    (if (null? deltas)
        'ok
        ((lambda ()
          (define delta (car deltas))
          (printf "=========delta ~a\n" delta)
          (define deltas-acc* (append deltas-acc (list delta)))

          (printf "===full ~a tuples \n" (set-count (apply-deltas deltas-acc* E0)))
          (define full-solver-results (map (lambda (solver) (printf ">>>\n") (solver P (apply-deltas deltas-acc* E0))) solvers))
          (check-equal-tuples* full-solver-results)
          (check-lesseq-derivations* full-solver-results)

          (printf "===single delta ~a\n" delta)
          (define single-delta-solver-results (map (lambda (delta-solver) (printf ">>>\n") (delta-solver (list delta))) delta-solvers))
          (check-equal-tuples (car full-solver-results) (car single-delta-solver-results))
          (check-equal-tuples* single-delta-solver-results)
          (check-lesseq-derivations* single-delta-solver-results)

          (printf "===acc deltas ~a\n" (set-count deltas-acc*))
          (define all-deltas-solver-results (map (lambda (delta-solver) (printf ">>>\n") (delta-solver deltas-acc*)) delta-solvers0))
          (check-equal-tuples (car full-solver-results) (car all-deltas-solver-results))
          (check-equal-tuples* all-deltas-solver-results)
          (check-lesseq-derivations* all-deltas-solver-results)

          (delta-loop (cdr deltas) deltas-acc* (map solver-result-delta-solver single-delta-solver-results)))))))


(define (performance-test P E0 solvers deltas)

  (define (data-point tme r)
    (cons tme (solver-result-num-derived-tuples r)))

  ; single-step: initial + each delta added individually using latest delta-solver
  (define (single-step)
    (for/list ((solver (in-list solvers)))
      (let* ((timer (create-timer))
              (initial-solver-result (solver P E0)))
        (let loop ((deltas deltas) (delta-solver (solver-result-delta-solver initial-solver-result)) (results (list (data-point (timer) initial-solver-result))))
          (if (null? deltas)
              (reverse results)
              (let ((delta (car deltas)))
                (let ((delta-solver-result (delta-solver (list delta))))
                  (loop (cdr deltas) (solver-result-delta-solver delta-solver-result) (cons (data-point (timer) delta-solver-result) results)))))))))

  (define (num-tuples-cumulator l) ; makes num-derived-tuples cumulative
    (define (helper l sum)
      (if (null? l)
          '()
          (match (car l)
            ((cons tm ndt)
              (let ((sum* (+ sum ndt)))
                (cons (cons tm sum*) (helper (cdr l) sum*)))))))
    (helper l 0))

  (define single-step-result (map num-tuples-cumulator (single-step)))

  (define (timings)
    (define (helper deltas sr1 sr2 sr3)
      (if (null? deltas)
          '()
          (let ((delta (car deltas))
                (t1 (caar sr1))
                (t2 (caar sr2))
                (t3 (caar sr3)))
            (cons (list delta t1 t2 t3) (helper (cdr deltas) (cdr sr1) (cdr sr2) (cdr sr3))))))
    (helper deltas (car single-step-result) (cadr single-step-result) (caddr single-step-result)))

  (define timings-result (timings))

  (define (print-csv ll)
    (for ((l ll))
      (for ((x l))
        (printf "~v; " x))
      (newline)))
  
  (print-csv timings-result)
  )