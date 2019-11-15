#lang racket

(require "datalog.rkt")

(provide correctness-test performance-test)

(define (create-timer)
  (let ((start (current-milliseconds)))
    (lambda ()
      (- (current-milliseconds) start))))

(define (correctness-test P E0 solvers . deltas)

  (define (check-equal-tuples*  rs)
    (for ((r2 (in-list (cdr rs))))
      (check-equal-tuples (car rs) r2)))

  (define (check-equal-tuples r1 r2)
    (unless (equal? (solver-result-tuples r1) (solver-result-tuples r2))
      (error "tuples not equal")))

  (define (check-lesseq-derivations* rs)
    (for ((r2 (in-list (cdr rs))))
      (check-lesseq-derivations (car rs) r2)))

  (define (check-lesseq-derivations r1 r2)
    (unless (> (solver-result-num-derived-tuples r1) (solver-result-num-derived-tuples r2))
      (error "num derived tuples not <=")))


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
        (let* ((delta (car deltas))
                (deltas-acc* (append deltas-acc (list delta)))
                (full-solver-results (map (lambda (solver) (solver P (apply-deltas deltas-acc* E0))) solvers))
                (single-delta-solver-results (map (lambda (delta-solver) (delta-solver delta)) delta-solvers))
                (all-deltas-solver-results (map (lambda (delta-solver) (apply delta-solver deltas-acc*)) delta-solvers0)))

          (check-equal-tuples (car full-solver-results) (car single-delta-solver-results))
          (check-equal-tuples (car full-solver-results) (car all-deltas-solver-results))


          (check-equal-tuples* full-solver-results)
          (check-lesseq-derivations* full-solver-results)
          
          (check-equal-tuples* single-delta-solver-results)
          (check-lesseq-derivations* single-delta-solver-results)
          
          (check-equal-tuples* all-deltas-solver-results)
          (check-lesseq-derivations* all-deltas-solver-results)

          (delta-loop (cdr deltas) deltas-acc* (map solver-result-delta-solver single-delta-solver-results))))))


(define (performance-test P E0 solvers . deltas)

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
                (let ((delta-solver-result (delta-solver delta)))
                  (loop (cdr deltas) (solver-result-delta-solver delta-solver-result) (cons (data-point (timer) delta-solver-result) results)))))))))


  (single-step)
)
