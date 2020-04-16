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
              (let ((derived-tuples-for-rule (for/set ((fr (in-set (fire-rule rule derived-tuples (set)))))
                                                (car fr)))) ; drop provenance
                ; (printf "fired ~a got ~a\n" rule derived-tuples-for-rule)
                (set! num-derived-tuples (+ num-derived-tuples (set-count derived-tuples-for-rule)))
                (loop (set-rest rules) (set-union derived-tuples derived-tuples-for-rule))))))) ; Accumulate all derived tuples.

    (define (stratum-loop S E-inter)
      ; (printf "\nn stratum ~a/~a with ~a tuples\n" (- (set-count strata) (set-count S)) (set-count strata) (set-count E-inter))
      (if (null? S) ; Check whether there are more strata to traverse.
          (solver-result E-inter num-derived-tuples (make-delta-solver)); All tuples (initial and derived).
          (let ((Pi (car S))) ; Rules in the first stratum.
            ; (printf "Pi: ~v\n" (list->set (set-map Pi (lambda (r) (atom-name (rule-head r))))))
            (let intra-loop ((E-intra E-inter))
              (let ((tuples (stratum-rule-loop Pi E-intra)))
                (let ((E-intra* (set-union E-intra tuples)))
                    (if (equal? E-intra E-intra*) ; monotonicity: size check quicker? (TODO)
                        (stratum-loop (cdr S) E-intra*)
                        (intra-loop E-intra*))))))))

    (define (make-delta-solver)
      (lambda msg 
        (match msg
          (`(apply-delta ,deltas)
            (let ((E (apply-deltas deltas E0)))
              (solve E)))
          (_ (error "incremental delta solver does not understand " msg)))))

    (stratum-loop strata E0))

  (solve E)                       
)


(module+ main

  (define r1 (:- #(Reachable x y)   #(Link x y)))
  (define r2 (:- #(Reachable x y)   #(Link x z) #(Reachable z y)))
  (define r3 (:- #(Node x)          #(Link x y)))
  (define r4 (:- #(Node y)          #(Link x y)))
  (define r5 (:- #(Unreachable x y) #(Node x) #(Node y) (¬ #(Reachable x y))))

  (define P (set r1 r2 r3 r4))
  ; (define P (set r1 r2 r3 r4 r5))
  (define E (set))

  (define deltas (list
    (add-tuple #(Link 'a 'b))
    (add-tuple #(Link 'b 'c))
    (add-tuple #(Link 'c 'c))
    (add-tuple #(Link 'c 'd))
    (add-tuple #(Link 'd 'e))
    (add-tuple #(Link 'e 'f))
    (remove-tuple #(Link 'e 'f))
    (add-tuple #(Link 'f 'g))
    (add-tuple #(Link 'g 'h))
    (add-tuple #(Link 'h 'i))
    (add-tuple #(Link 'm 'n))
    (add-tuple #(Link 'l 'm))
    (remove-tuple #(Link 'f 'g))
    (remove-tuple #(Link 'b 'c))
    (add-tuple #(Link 'k 'l))
    (add-tuple #(Link 'j 'k))
    (add-tuple #(Link 'k 'k))
    (add-tuple #(Link 'i 'j))
    (add-tuple #(Link 'o 'p))
    (remove-tuple #(Link 'h 'i))
    (add-tuple #(Link 'q 'r))
    (add-tuple #(Link 'p 'q))
    (remove-tuple #(Link 'j 'k))
    (remove-tuple #(Link 'k 'l))
    (add-tuple #(Link 's 'u))
    (add-tuple #(Link 's 't))
    (add-tuple #(Link 't 'u))
    (remove-tuple #(Link 'a 'b))
    (add-tuple #(Link 'v 'w))
    (remove-tuple #(Link 'p 'q))
    (remove-tuple #(Link 'o 'p))
    (add-tuple #(Link 'v 'x))
    (remove-tuple #(Link 'k 'k))
    (remove-tuple #(Link 's 'u))
    (remove-tuple #(Link 'm 'n))
    (remove-tuple #(Link 'l 'm))
    (add-tuple #(Link 'w 'x))
    (remove-tuple #(Link 'c 'd))
    (add-tuple #(Link 'y 'y))
    (remove-tuple #(Link 's 't))
    (remove-tuple #(Link 'g 'h))
    (add-tuple #(Link 'y 'x))
    (remove-tuple #(Link 'i 'j))
    (add-tuple #(Link 'y 'z))
    (remove-tuple #(Link 'c 'c))
    (remove-tuple #(Link 't 'u))
    (add-tuple #(Link 'z 'a))
    (remove-tuple #(Link 'q 'r))
    (remove-tuple #(Link 'd 'e))
    (remove-tuple #(Link 'v 'w))
    (remove-tuple #(Link 'v 'x))
    (remove-tuple #(Link 'w 'x))
    (remove-tuple #(Link 'y 'y))
    (remove-tuple #(Link 'y 'x))
    (remove-tuple #(Link 'y 'z))
    (remove-tuple #(Link 'z 'a))
  ))

  (let ((result
      (for/fold ((solver (solve-naive P E))) ((delta deltas))
        (define solver* (solver-result-delta-solver solver))
        (solver* 'apply-delta (list delta)))))
    (printf "~a\n" (sort-tuples (solver-result-tuples result))))
)

