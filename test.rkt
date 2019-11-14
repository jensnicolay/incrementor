#lang racket

(require "datalog.rkt")

(provide correctness-test performance-test)

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
        'done
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
  123

  ; (define delta-acc-list
  ;   (for/fold ((deltas-acc (list E0))) ((delta (in-list deltas)))
  ;     (append deltas-acc (list delta))))

  ; (define naive (solve-naive P E0)) ; TODO make output versions to help GC
  ; (define semi-naive (solve-semi-naive P E0))
  ; (define incremental (solve-semi-naive-i P E0))

  ; (check-equal-tuples naive semi-naive)
  ; (check-equal-tuples naive incremental)

  ; (check-lesseq-derivations semi-naive naive)
  ; (check-lesseq-derivations incremental naive)

  ; (define naive-solver0 (solver-result-solver naive))
  ; (define semi-naive-solver0 (solver-result-solver semi-naive))
  ; (define incremental-solver0 (solver-result-solver incremental))

  ; (define (delta-solver-apply-list-of-deltas delta-solver deltas)
  ;   ()

  ; (define (replay initial-solver list-of-list-of-deltas)
  ;   (let ((start (current-milliseconds)))
  ;     (let ((solver0 (initial-solver P E0)))
  ;       (let ((end (- (current-milliseconds) start)))
  ;         (let ll-deltas-loop ((ll-deltas list-of-list-of-deltas) (result (list solver0))) ; TODO map output transf
  ;     (if (null? delta-acc-list)
  ;         (list->vector (reverse results))
  ;         (let ((delta-acc (car delta-acc-list)))
  ;           (printf "deltas ~a/~a: ~a\n" (- (length deltas) (length delta-acc-list)) (length deltas))
  ;           (let ((start (current-milliseconds)))
  ;             (for ((deltas (in-list delta-acc)))
  ;           (for/vector ((solver (in-vector solvers)))
  ;           (for/vector ((  (in-list delta-acc*)))
  ;             (solver )
  ;           (match-let (((solver-result tuples-naive duration-naive num-derived-naive _) (solve-naive P E-acc*))
  ;                       ((solver-result tuples-semi-naive duration-semi-naive num-derived-semi-naive _) (solve-semi-naive P E-acc*))
  ;                       ((solver-result tuples-semi-naive-i-single duration-semi-naive-i-single num-derived-semi-naive-i-single _) (incremental-solver-single (set delta)))
  ;                       ((solver-result tuples-semi-naive-i-acc duration-semi-naive-i-acc num-derived-semi-naive-i-acc _) (incremental-solver0 deltas-acc*))
  ;                     )

  ;           (check-equal-tuples naive semi-naive)
  ;           (check-equal-tuples naive incremental)

  ;           (check-lesseq-derivations semi-naive naive)
  ;           (check-lesseq-derivations incremental naive)

  ;           (delta-loop (cdr deltas) deltas-acc* E-acc*)))))))


  ; (define replay-results (replay (vector naive-solver0 semi-naive-solver0 incremental-solver0)))

)

; TODO: the single step thingies should be rerun to avoid accumulating time rounding errors