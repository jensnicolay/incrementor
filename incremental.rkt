#lang racket 

(require "datalog.rkt")
(provide solve-semi-naive-i)

(define (solve-semi-naive-i P E)

  (define strata (stratify P))

  (define (solve E)

    (define num-derived-tuples 0)

    (define (stratum-rule-loop rules tuples) ; per stratum, initial

      (define edb-rules (get-edb-rules rules))
      ;(printf "edb-rules ~a\n" edb-rules)
      (define semi-naive-rules-idb (rewrite-semi-naive-idb rules))
      ;(printf "semi-naive rules idb: ~a\n" semi-naive-rules-idb)
      (define p->r (pred-to-rules semi-naive-rules-idb))
      ;(printf "pred-to-rules ~a\n" p->r)
      (define num-derived-tuples 0)
      
      (let delta-loop ((delta-rules edb-rules) (tuples tuples) (previous-delta-tuples tuples))
        ;(printf "delta rules :~a\n" delta-rules)
        (let delta-rule-loop ((rules* delta-rules) (delta-tuples (set)))
          (if (set-empty? rules*)
            (let ((real-delta-tuples (set-subtract delta-tuples tuples))) ; TODO?: full tuples set subtr
              ;(printf "real-delta-tuples ~a\n" real-delta-tuples)
              (if (set-empty? real-delta-tuples)
                  tuples
                  (delta-loop (select-rules-for-tuples real-delta-tuples p->r) (set-union tuples real-delta-tuples) real-delta-tuples)))
            (let ((rule (set-first rules*)))
              (let ((derived-tuples-for-rule (fire-rule rule tuples previous-delta-tuples)))
                ;(printf "fired ~a got ~a\n" rule derived-tuples-for-rule)
                (set! num-derived-tuples (+ num-derived-tuples (set-count derived-tuples-for-rule)))
                (delta-rule-loop (set-rest rules*) (set-union delta-tuples derived-tuples-for-rule))))))))


    (define (stratum-loop S E)
      ;(printf "\ni stratum ~a/~a with ~a tuples\n" (- (set-count strata) (set-count S)) (set-count strata) (set-count E))
      (if (null? S)
          (solver-result E num-derived-tuples (make-delta-solver E))
          (let ((E* (stratum-rule-loop (car S) E)))
            (stratum-loop (cdr S) E*))))

    (stratum-loop strata E)) ; end solve

  (define (solve-incremental E tuples-add)
    
    (define num-derived-tuples 0)

    (define (stratum-rule-loop rules tuples real-delta-tuples-edb) ; per stratum, incremental
      (define semi-naive-rules-edb (rewrite-semi-naive-edb rules)) ; TODO move so this work is only done once
      ;(printf "semi-naive rules edb: ~a\n" semi-naive-rules-edb)
      (define semi-naive-rules-idb (rewrite-semi-naive-idb rules))
      ;(printf "semi-naive rules idb: ~a\n" semi-naive-rules-idb)
      (define p->r-idb (pred-to-rules semi-naive-rules-idb))
      (define p->r-edb (pred-to-rules semi-naive-rules-edb))
      ;(printf "pred-to-rules ~a\n" p->r)

      ; TODO (here, elsewhere?): adding (re)discovered tuples immediately to a current `all-tuples` set (quicker convergence?)

      (let delta-rule-loop-edb ((edb-rules* (select-rules-for-tuples real-delta-tuples-edb p->r-edb)) (delta-tuples (set)))
        (if (set-empty? edb-rules*)
          
          (let ((real-delta-tuples-idb (set-subtract delta-tuples tuples))) ; TODO?: full tuples set subtr
            ;(printf "*real-delta-tuples-idb ~a\n" real-delta-tuples-idb)
            (let delta-loop-idb ((delta-rules (select-rules-for-tuples real-delta-tuples-idb p->r-idb)) (tuples (set-union tuples real-delta-tuples-edb real-delta-tuples-idb)) (previous-delta-tuples real-delta-tuples-idb))
              ;(printf "tuples: (~a) ~a\n" (set-count tuples) tuples)
              (let delta-rule-loop-idb ((idb-rules* delta-rules) (delta-tuples (set)))
                (if (set-empty? idb-rules*)
                  (let ((real-delta-tuples-idb (set-subtract delta-tuples tuples)))
                    ;(printf "real-delta-tuples-idb ~a\n" real-delta-tuples-idb)
                    (if (set-empty? real-delta-tuples-idb)
                        tuples
                        (delta-loop-idb (select-rules-for-tuples real-delta-tuples-idb p->r-idb) (set-union tuples real-delta-tuples-idb) real-delta-tuples-idb)))
                  (let ((rule (set-first idb-rules*)))
                    (let ((derived-tuples-for-rule (fire-rule rule tuples previous-delta-tuples)))
                      ;(printf "fired ~a got ~a\n" rule derived-tuples-for-rule)
                      (set! num-derived-tuples (+ num-derived-tuples (set-count derived-tuples-for-rule)))
                      (delta-rule-loop-idb (set-rest idb-rules*) (set-union delta-tuples derived-tuples-for-rule))))))))
                      
            (let ((rule (set-first edb-rules*)))
                    (let ((derived-tuples-for-rule (fire-rule rule tuples real-delta-tuples-edb)))
                      ;(printf "fired ~a got ~a\n" rule derived-tuples-for-rule)
                      (set! num-derived-tuples (+ num-derived-tuples (set-count derived-tuples-for-rule)))
                      (delta-rule-loop-edb (set-rest edb-rules*) (set-union delta-tuples derived-tuples-for-rule))))
                      )))

    (define (stratum-loop S E)
        ;(printf "\ni stratum ~a/~a with ~a tuples\n" (- (set-count strata) (set-count S)) (set-count strata) (set-count E))
        (if (null? S)
            (solver-result E num-derived-tuples (make-delta-solver E))
            (let ((E* (stratum-rule-loop (car S) E tuples-add)))
              (stratum-loop (cdr S) E*))))

    (stratum-loop strata E))

  (define (make-delta-solver E)
    (lambda deltas
      (let-values (((tuples-add tuples-remove)
        (for/fold ((tuples-add (set)) (tuples-remove (set))) ((delta (in-list deltas)))
          (match delta
            ((add-tuple tuple) (values (set-add tuples-add tuple) tuples-remove))
            ((remove-tuple tuple) (values tuples-add (set-add tuples-remove tuple)))))))
        (solve-incremental E tuples-add))))

  (solve E))

; rewrites IDB preds in rules that contain them
(define (rewrite-semi-naive-idb rules)
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

; rewrites EDB preds in rules that contain them
(define (rewrite-semi-naive-edb rules)
  (let ((idb-preds (for/set ((rule (in-set rules)))
                      (atom-name (rule-head rule))))) ; this corresponds to Strata?

  (define (rewrite-rule r)
    (match-let (((rule head body) r))
      
    (define (rewrite-terms previous-terms future-terms rewrites)
      (if (null? future-terms)
          rewrites ; rules without EDB preds are filtered out
          (let ((term (car future-terms)))
            (match term
              ((¬ p) ; in a stratified Datalog, all negated preds are guaranteed to be EDBs
                (rewrite-terms (cons term previous-terms) (cdr future-terms) (set-add rewrites (rule head (append (reverse previous-terms) (list (¬ `#(*Recent* ,term))) (cdr future-terms))))))
              (_
                (let ((name (atom-name term)))
                  (if (set-member? idb-preds name)
                      (rewrite-terms (cons term previous-terms) (cdr future-terms) rewrites)
                      (rewrite-terms (cons term previous-terms) (cdr future-terms) (set-add rewrites (rule head (append (reverse previous-terms) (list `#(*Recent* ,term)) (cdr future-terms))))))))))))

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
            (if (equal? term-name '*Recent*)
                (let ((recent-term (vector-ref term 1)))
                  (let ((recent-name (atom-name recent-term)))
                    (hash-set R recent-name (set-add (hash-ref R recent-name (set)) r))))
                (hash-set R term-name (set-add (hash-ref R term-name (set)) r)))))
        (_ 
          (let ((term-name (atom-name term)))
            (if (equal? term-name '*Recent*)
                (let ((recent-term (vector-ref term 1)))
                  (let ((recent-name (atom-name recent-term)))
                    (hash-set R recent-name (set-add (hash-ref R recent-name (set)) r))))
                (hash-set R term-name (set-add (hash-ref R term-name (set)) r)))))
        )))) 

; in: set of tuples and map of pred->rules
; out: all rules that need to be fired (based on the preds in rules' body)
(define (select-rules-for-tuples tuples p->r)
  (for/fold ((R (set))) ((tuple (in-set tuples)))
    (set-union R (hash-ref p->r (atom-name tuple) (set)))))

; selects rules that only contain EDB preds
; only used for initial EDB->IDB, when "existing" edb-tuples = (set) and all initial EDB tuples are considered deltas
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
    
     

