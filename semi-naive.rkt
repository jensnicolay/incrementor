#lang racket 

(require "datalog.rkt")
(provide solve-semi-naive)

(define (solve-semi-naive P E)

  (define strata (stratify P))

  (let stratum-loop ((S strata) (E E))
    (printf "\ninter ~a/~a with ~a tuples\n" (- (set-count strata) (set-count S)) (set-count strata) (set-count E))

    (if (null? S)
        E
        (let ((E* (perform-iter (car S) E)))
          (stratum-loop (cdr S) E*)))))

(define (rewrite-semi-naive rules)
  (let ((idb-preds (for/set ((rule (in-set rules)))
                      (atom-name (rule-head rule))))) ; this corresponds to Strata?

  (define (rewrite-rule r)
    (match-let (((rule head body) r))
      
    (define (rewrite-terms previous-terms future-terms rewrites)
      (if (null? future-terms)
          (if (set-empty? rewrites)
              (set r)
              rewrites)
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


(define (perform-iter rules tuples)

  (define semi-naive-rules (rewrite-semi-naive rules))
  (printf "semi-naive rules: ~a\ntuples: ~a\n" semi-naive-rules tuples)
  (define p->r (pred-to-rules semi-naive-rules))
  ;(printf "pred-to-rules ~a\n" p->r)
  
  (let rule-loop ((rules* semi-naive-rules) (tuples tuples) (previous-delta-tuples tuples) (delta-tuples (set)))
    (let ((delta-rules (select-rules-for-tuples previous-delta-tuples p->r)))
      ;(printf "delta rules :~a\n" delta-rules)
      (let delta-rule-loop ((rules* delta-rules) (delta-tuples delta-tuples))
        (if (set-empty? rules*)
          (let ((real-delta-tuples (set-subtract delta-tuples tuples)))
            (printf "delta-tuples ~a\nreal-delta-tuples ~a\n" delta-tuples real-delta-tuples)
            (if (set-empty? real-delta-tuples)
                tuples
                (rule-loop semi-naive-rules (set-union tuples real-delta-tuples) real-delta-tuples (set))))
          (let ((rule (set-first rules*)))
            (let ((derived-tuples-for-rule (fire-rule rule tuples previous-delta-tuples)))
              ;(printf "fired ~a got ~a\n" rule derived-tuples-for-rule)
              (delta-rule-loop (set-rest rules*) (set-union delta-tuples derived-tuples-for-rule)))))))))
