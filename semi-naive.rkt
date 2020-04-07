#lang racket 

(require "datalog.rkt")
(provide solve-semi-naive)

(struct stratum (edb-rules p->r) #:transparent)

(define (solve-semi-naive P E)

  (define strata (map annotate-stratum (stratify P)))

  (define (solve E0)
    
    (define num-derived-tuples 0)

    (define (stratum-rule-loop strat tuples) ; per stratum

      (define edb-rules (stratum-edb-rules strat))
      (define p->r (stratum-p->r strat))
      
      (let rule-loop ((delta-rules edb-rules) (tuples tuples) (previous-delta-tuples tuples))
        (printf "delta rules :~a\n" delta-rules)
        (let delta-rule-loop ((rules* delta-rules) (delta-tuples (set)))
          (if (set-empty? rules*)
            (let ((real-delta-tuples (set-subtract delta-tuples tuples))) ; TODO?: full set subtr
              (printf "real-delta-tuples ~a\n" real-delta-tuples)
              (if (set-empty? real-delta-tuples)
                  tuples
                  (rule-loop (select-rules-for-tuples real-delta-tuples p->r) (set-union tuples real-delta-tuples) real-delta-tuples)))
            (let ((rule (set-first rules*)))
              (let ((derived-tuples-for-rule (for/set ((fr (in-set (fire-rule rule tuples previous-delta-tuples))))
                                                (car fr)))) ; drop provenance
                (printf "fired ~a got ~a\n" rule derived-tuples-for-rule)
                (set! num-derived-tuples (+ num-derived-tuples (set-count derived-tuples-for-rule)))
                (delta-rule-loop (set-rest rules*) (set-union delta-tuples derived-tuples-for-rule))))))))

  (define (stratum-loop S E)
    (printf "\nsn stratum ~a/~a with ~a tuples\n" (- (set-count strata) (set-count S)) (set-count strata) (set-count E))
    (if (null? S)
        (solver-result E num-derived-tuples (make-delta-solver))
        (let ((E* (stratum-rule-loop (car S) E)))
          (stratum-loop (cdr S) E*))))

  (define (make-delta-solver) ; TODO: investigate: addition-only delta can be computed more optimal by keeping all tuples?
    (lambda (deltas)
      (let ((E (apply-deltas deltas E0)))
        ; (printf "solving ~a\n" E)
        (solve E))))
  
  (stratum-loop strata E0))
  
  (solve E))

(define (rewrite-semi-naive rules)
  (let ((idb-preds (for/set ((rule (in-set rules)))
                      (atom-name (rule-head rule))))) ; this corresponds to Strata?

  (define (rewrite-rule r)
    (match-let (((rule head body) r))
      
    (define (rewrite-terms previous-terms future-terms rewrites)
      (if (null? future-terms) 
          rewrites ; rules without IDB preds are filtered out
          (let ((term (car future-terms)))
            (match term
              ((¬ p) ; in a stratified Datalog, all negated preds are guaranteed to be EDBs, so no action necessary
                (rewrite-terms (cons term previous-terms) (cdr future-terms) rewrites))
              (_
                (let ((name (atom-name term)))
                  (if (set-member? idb-preds name)
                      (rewrite-terms (cons term previous-terms) (cdr future-terms) (set-add rewrites (rule head (append (reverse previous-terms) (list `#(*Recent* ,term)) (cdr future-terms)))))
                      (rewrite-terms (cons term previous-terms) (cdr future-terms) rewrites))))))))

    ; (define (rewrite-term term previous-terms future-terms)
    ;   (cons head (append previous-terms (list `#(*Recent* ,term)) future-terms)))
    
    (rewrite-terms '() body (set))))

    (for/fold ((rewritten-rules (set))) ((r (in-set rules)))
      (let ((rewrites (rewrite-rule r)))
        (set-union rewritten-rules rewrites)))))

; in: rules
; out: mapping pred -> rules with that pred in body
(define (pred-to-rules rules)
  (for/fold ((R (hash))) ((r (in-set rules)))
    (for/fold ((R R)) ((term (in-list (rule-body r))))
      (match term
        ((¬ p)
          (let ((term-name (atom-name p)))
          (hash-set R term-name (set-add (hash-ref R term-name (set)) r)))) ; remember: no *Recent* possible!
        (_ 
          (let ((term-name (atom-name term)))
            (if (equal? term-name '*Recent*)
                (let ((recent-term (vector-ref term 1)))
                  (let ((recent-name (atom-name recent-term)))
                    (hash-set R recent-name (set-add (hash-ref R recent-name (set)) r))))
                (hash-set R term-name (set-add (hash-ref R term-name (set)) r)))))
        )))) ; always an EDB?

; in: set of tuples and map of pred->rules
; out: all rules that need to be fired (based on the preds in rules' body)
(define (select-rules-for-tuples tuples p->r)
  (for/fold ((R (set))) ((tuple (in-set tuples)))
    (set-union R (hash-ref p->r (atom-name tuple) (set)))))

(define (get-edb-rules rules) ; these don't need to be rewritten
  (let ((idb-preds
          (for/set ((r (in-set rules)))
            (atom-name (rule-head r))))); this again is already computed by scc (TODO: stratum abstraction!)
    (for/fold ((R (set))) ((r (in-set rules)))
      (if (for/and ((term (in-list (rule-body r))))
            (match term
              ((¬ p) ; must be edb!
                #t)
              (_
                (let ((term-name (atom-name term)))
                  (not (set-member? idb-preds term-name))))))
          (set-add R r)
          R))))

(define (annotate-stratum stratum-rules)
  (define semi-naive-rules (rewrite-semi-naive stratum-rules))
  ;(printf "semi-naive rules: ~a\n" semi-naive-rules)
  (define p->r (pred-to-rules semi-naive-rules))
  (printf "pred-to-rules\n")(print-map p->r)
  (define edb-rules (get-edb-rules stratum-rules))
  ;(printf "edb-rules ~a\n" edb-rules)

  (stratum edb-rules p->r))