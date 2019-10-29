#lang racket 

(require "datalog.rkt")
(provide solve-semi-naive-i solver-result-i apply-deltas)

(struct solver-result-i solver-result (incremental-solver) #:transparent)
(struct add (tuple) #:transparent)
(struct remove (tuple) #:transparent)

(define (apply-deltas deltas E)
  (for/fold ((E E)) ((delta deltas))
    (match delta
      ((add tuple) (set-add E tuple))
      ((remove tuple) (set-remove E tuple)))))

(define (solve-semi-naive-i P E)

  (define start (current-milliseconds))

  (define strata (stratify P))

  (let stratum-loop ((S strata) (E E) (num-derived-tuples 0))
    (printf "\nstratum ~a/~a with ~a tuples\n" (- (set-count strata) (set-count S)) (set-count strata) (set-count E))

    (if (null? S)
        (solver-result-i
          (make-solver P E)
          E (- (current-milliseconds) start) num-derived-tuples
        )
        (let-values (((E* num-derived-tuples*) (perform-iter (car S) (set) E)))
          (stratum-loop (cdr S) E* (+ num-derived-tuples num-derived-tuples*))))))

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

; selects rules that only contain EDB preds
; (define (get-edb-rules rules) ; these don't need to be rewritten
;   (let ((idb-preds
;           (for/set ((r (in-set rules)))
;             (atom-name (rule-head r))))); this again is already computed by scc (TODO: stratum abstraction!)
;     (for/fold ((R (set))) ((r (in-set rules)))
;       (if (for/and ((term (in-list (rule-body r))))
;             (match term
;               ((¬ p) ; must be edb!
;                 #t)
;               (_
;                 (let ((term-name (atom-name term)))
;                   (not (set-member? idb-preds term-name))))))
;           (set-add R r)
;           R))))

(define (make-solver P E)
  (lambda (deltas)
    (let-values (((tuples-add tuples-remove)
      (for/fold ((tuples-add (set)) (tuples-remove (set))) ((delta (in-set deltas)))
        (match delta
          ((add tuple) (values (set-add tuples-add tuple) tuples-remove))
          ((remove tuple) (values tuples-add (set-add tuples-remove tuple)))))))
      (perform-iter tuples-add E))))
      

(define (perform-iter rules tuples real-delta-tuples) ; per stratum

  (define semi-naive-rules (rewrite-semi-naive rules)) ; TODO: needs to be done once!
  (printf "semi-naive rules: ~a\n" semi-naive-rules)
  (define p->r (pred-to-rules semi-naive-rules))
  ;(printf "pred-to-rules ~a\n" p->r)
  (define num-derived-tuples 0)
  
  (define (iterare real-delta-tuples tuples)
    (printf "real-delta-tuples ~a\n" real-delta-tuples)
    (if (set-empty? real-delta-tuples)
        (values tuples num-derived-tuples)
        (let ((delta-rules (select-rules-for-tuples real-delta-tuples p->r)))
          (printf "rules for real-deltas: ~a\n" delta-rules)
          (let ((tuples* (set-union tuples real-delta-tuples)))
            (let delta-rule-loop ((rules* delta-rules) (delta-tuples (set)))
              (if (set-empty? rules*)
                (let ((real-delta-tuples (set-subtract delta-tuples tuples*)))
                  (iterare real-delta-tuples tuples*))
                (let ((rule (set-first rules*)))
                  (let ((derived-tuples-for-rule (fire-rule rule tuples* real-delta-tuples)))
                    (printf "fired ~a got ~a\n" rule derived-tuples-for-rule)
                    (set! num-derived-tuples (+ num-derived-tuples (set-count derived-tuples-for-rule)))
                    (delta-rule-loop (set-rest rules*) (set-union delta-tuples derived-tuples-for-rule))))))))))
  
  (iterare real-delta-tuples tuples))
    
     
