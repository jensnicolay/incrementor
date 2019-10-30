#lang racket 

(require "datalog.rkt")
(provide solve-semi-naive-i solver-result-i add-tuple remove-tuple apply-deltas)

(struct solver-result-i solver-result (incremental-solver) #:transparent)
(struct add-tuple (tuple) #:transparent)
(struct remove-tuple (tuple) #:transparent)

(define (apply-deltas deltas E)
  (for/fold ((E E)) ((delta deltas))
    (match delta
      ((add-tuple tuple) (set-add E tuple))
      ((remove-tuple tuple) (set-remove E tuple)))))

(define (solve-semi-naive-i P E)

  (define start (current-milliseconds))

  (define strata (stratify P))

  (let stratum-loop ((S strata) (E E) (num-derived-tuples 0))
      (printf "\ni stratum ~a/~a with ~a tuples\n" (- (set-count strata) (set-count S)) (set-count strata) (set-count E))

      (if (null? S)
          (solver-result-i #f E (- (current-milliseconds) start) num-derived-tuples)
          (let-values (((E* num-derived-tuples*) (perform-iter (car S) E)))
            (stratum-loop (cdr S) E* (+ num-derived-tuples num-derived-tuples*))))))

(define (make-solver P E)
  (lambda (deltas) 'ok))
    ; (let-values (((tuples-add tuples-remove)
    ;   (for/fold ((tuples-add (set)) (tuples-remove (set))) ((delta (in-set deltas)))
    ;     (match delta
    ;       ((add-tuple tuple) (values (set-add tuples-add tuple) tuples-remove))
    ;       ((remove-tuple tuple) (values tuples-add (set-add tuples-remove tuple)))))))
    ;   (solve P E tuples-add))))

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
(define (get-edb-rules rules) ; these don't need to be rewritten (OR: should always be rewritten to *Recent* so they only work with incoming delta-edb-tuples)
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

(define (perform-iter rules tuples) ; per stratum

  (define semi-naive-rules (rewrite-semi-naive rules))
  (printf "semi-naive rules: ~a\n" semi-naive-rules)
  (define p->r (pred-to-rules semi-naive-rules))
  ;(printf "pred-to-rules ~a\n" p->r)
  (define edb-rules (get-edb-rules rules))
  ;(printf "edb-rules ~a\n" edb-rules)
  (define num-derived-tuples 0)
  
  (let rule-loop ((delta-rules edb-rules) (tuples tuples) (previous-delta-tuples tuples))
    ;(printf "delta rules :~a\n" delta-rules)
    (let delta-rule-loop ((rules* delta-rules) (delta-tuples (set)))
      (if (set-empty? rules*)
        (let ((real-delta-tuples (set-subtract delta-tuples tuples)))
          (printf "delta-tuples ~a\nreal-delta-tuples ~a\n" delta-tuples real-delta-tuples)
          (if (set-empty? real-delta-tuples)
              (values tuples num-derived-tuples)
              (rule-loop (select-rules-for-tuples real-delta-tuples p->r) (set-union tuples real-delta-tuples) real-delta-tuples)))
        (let ((rule (set-first rules*)))
          (let ((derived-tuples-for-rule (fire-rule rule tuples previous-delta-tuples)))
            ;(printf "fired ~a got ~a\n" rule derived-tuples-for-rule)
            (set! num-derived-tuples (+ num-derived-tuples (set-count derived-tuples-for-rule)))
            (delta-rule-loop (set-rest rules*) (set-union delta-tuples derived-tuples-for-rule))))))))
    
     