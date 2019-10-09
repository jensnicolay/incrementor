#lang racket

(require "datalog.rkt")
(provide solve-naive)

(define (solve-naive P E)

  (define (solve-naive-helper rules tuples)
    (let loop ((rules rules) (derived-tuples tuples))
      (if (set-empty? rules) ; Check whether all rules have been traversed as far as needed.
          derived-tuples
          (let ((rule (set-first rules)))
            (let ((derived-tuples-for-rule (fire-rule rule derived-tuples (set)))) ; Fire the first rule.
              (loop (set-rest rules) (set-union derived-tuples derived-tuples-for-rule))))))) ; Accumulate all derived tuples.

  (define strata (stratify P))
  ;(printf "stratify ~v\n\n" strata)

  ; * E      set of tuples (initially only the ones in the database)
  ; * strata list of strata
  (let inter-loop ((E-inter E) (S strata))
    (printf "\ninter ~a/~a with ~a tuples\n" (- (set-count strata) (set-count S)) (set-count strata) (set-count E-inter))
    (if (null? S) ; Check whether there are more strata to traverse.
        E-inter ; All tuples (initial and derived).
        (let ((Pi (car S))) ; Rules in the first stratum.
          (printf "Pi: ~v\n" (list->set (set-map Pi (lambda (r) (atom-name (rule-head r))))))
          (let intra-loop ((E-intra E-inter))
            (let ((tuples (solve-naive-helper Pi E-intra)))
              (let ((E-intra* (set-union E-intra tuples)))
                  (if (equal? E-intra E-intra*) ; monotonicity: size check quicker? (TODO)
                      (inter-loop E-intra* (cdr S))
                      (let ((new-tuples (set-subtract E-intra* E-intra)))
                        (printf "new tuples: ~a\n" new-tuples)
                        (intra-loop E-intra*))))))))))
