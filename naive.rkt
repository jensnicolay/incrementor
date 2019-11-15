#lang racket

(require "datalog.rkt")
(provide solve-naive)


(define (solve-naive P E)

  (define strata (stratify P))
  
  ; * E      set of tuples (initially only the ones in the database)
  (define (solve E0)

    (define num-derived-tuples 0)

    (define (stratum-rule-loop rules tuples) ; per stratum
      (let loop ((rules rules) (derived-tuples tuples))
        (if (set-empty? rules) ; Check whether all rules have been traversed as far as needed.
            derived-tuples
            (let ((rule (set-first rules)))
              (let ((derived-tuples-for-rule (fire-rule rule derived-tuples (set)))) ; Fire the first rule.
                (set! num-derived-tuples (+ num-derived-tuples (set-count derived-tuples-for-rule)))
                (loop (set-rest rules) (set-union derived-tuples derived-tuples-for-rule))))))) ; Accumulate all derived tuples.

    (define (stratum-loop S E-inter)
      ;(printf "\nn stratum ~a/~a with ~a tuples\n" (- (set-count strata) (set-count S)) (set-count strata) (set-count E-inter))
      (if (null? S) ; Check whether there are more strata to traverse.
          (solver-result E-inter num-derived-tuples (make-delta-solver)); All tuples (initial and derived).
          (let ((Pi (car S))) ; Rules in the first stratum.
            ;(printf "Pi: ~v\n" (list->set (set-map Pi (lambda (r) (atom-name (rule-head r))))))
            (let intra-loop ((E-intra E-inter))
              (let ((tuples (stratum-rule-loop Pi E-intra)))
                (let ((E-intra* (set-union E-intra tuples)))
                    (if (equal? E-intra E-intra*) ; monotonicity: size check quicker? (TODO)
                        (stratum-loop (cdr S) E-intra*)
                        (intra-loop E-intra*))))))))

    (define (make-delta-solver)
      (lambda (deltas)
        (let ((E (apply-deltas deltas E0)))
          (solve E))))
                                            
    (stratum-loop strata E0))

  (solve E)                       
)
