#lang racket 

(require "datalog.rkt")
(require "provenance.rkt")
(provide solve-incremental)

(struct stratum (edb-rules p->r-edb p->r-edb¬ p->r-idb) #:transparent)

(define (update-provenance provenance tuple provenance-product)
  (if (set-member? provenance-product tuple) ; remove direct recursion
      provenance
      (let ((current-prov (hash-ref provenance tuple (set))))
        (let ((updated-prov (add-product current-prov provenance-product))) ; remember: do not recursively simplify here (only top-level absorption + direct recursion removal): need to have "raw" provenance!
          ;(printf "update prov ~a ~a\n~a -> ~a\n\n" tuple provenance-product current-prov updated-prov)
          (hash-set provenance tuple updated-prov)))))


(define (solve-incremental P E)

  (define strata (map annotate-stratum (stratify P)))

  (define (solve-incremental-initial E0) ; E0 are the initial EDBs

    (define num-derived-tuples 0)
    ;(define provenance (hash)) ; TODO: why not do prov per stratum (smaller, faster maps)?
    (define provenance-simplified ; TODO: insight: only add incrementality for initial solve (i.e., no deltas)
      (for/hash ((tuple-add (in-set E0)))
        (values tuple-add (set (set tuple-add)))))

    ;(printf "initial prov:\n")
    ;(print-map provenance)
    ; TODO: more general: provenance over strata or per stratum?

    (define (stratum-rule-loop strat tuples provenance) ; per stratum, initial

      (define edb-rules (stratum-edb-rules strat)) ; TODO: maybe also involve select-rules-f-t (but only select from edb-only rules)
      (define p->r (stratum-p->r-idb strat))
      
      (let delta-loop ((delta-rules edb-rules) (tuples tuples) (previous-delta-tuples tuples) (provenance provenance))
        (printf "delta rules :~a\n" delta-rules)
        (printf "tuples :~a\n" tuples)
        (printf "previous delta tuples :~a\n" previous-delta-tuples)
        (let delta-rule-loop ((rules* delta-rules) (delta-tuples (set)) (provenance provenance))
          (if (set-empty? rules*)
            (let ((real-delta-tuples (set-subtract delta-tuples tuples))) ; TODO?: full tuples set subtr
              (printf "real-delta-tuples ~a\n" real-delta-tuples)
              (if (set-empty? real-delta-tuples)
                  (values tuples provenance)
                  (delta-loop (select-rules-for-tuples real-delta-tuples p->r) (set-union tuples real-delta-tuples) real-delta-tuples provenance)))
            (let ((rule (set-first rules*)))
              (let ((derived-tuples-with-provenance-for-rule (fire-rule rule tuples previous-delta-tuples)))
                (printf "fired ~a got ~a\n" rule derived-tuples-with-provenance-for-rule)
                (let-values (((derived-tuples-for-rule provenance)
                    (for/fold ((derived-tuples-for-rule (set)) (provenance provenance)) ((fr (in-set derived-tuples-with-provenance-for-rule)))
                      (match-let (((cons derived-tuple prov) fr))
                        (values (set-add derived-tuples-for-rule derived-tuple) (update-provenance provenance derived-tuple prov))))))
                  (set! num-derived-tuples (+ num-derived-tuples (set-count derived-tuples-for-rule)))
                  (delta-rule-loop (set-rest rules*) (set-union delta-tuples derived-tuples-for-rule) provenance))))))))


    (define (stratum-loop S E provenance)
      (printf "\ni stratum ~a/~a with ~a tuples\n" (- (set-count strata) (set-count S)) (set-count strata) (set-count E))
      (if (null? S)
          (solver-result E num-derived-tuples (make-delta-solver E0 provenance)) ; E0 are the initial EDBs, prov keys are all derived IDBs
          (let-values (((E* provenance) (stratum-rule-loop (car S) E provenance)))
            (stratum-loop (cdr S) E* provenance))))

    (stratum-loop strata E0 (hash))
  ) ; end solve initial


  (define (solve-incremental-delta E0 provenance tuples-add* tuples-remove*) ; E0 are previous initial EDBs, prov keys are all derived IDBs,  tuples-add/remove are initial delta EDBs
    
    (define num-derived-tuples 0)

    (define (stratum-rule-loop strat tuples real-delta-tuples-edb provenance) ; per stratum, incremental

      (printf "\nstratum-rule-loop real-delta-tuples-edb ~a\n" real-delta-tuples-edb)

      (define p->r-edb (stratum-p->r-edb strat))
      (define p->r-edb¬ (stratum-p->r-edb¬ strat))
      (define p->r-idb (stratum-p->r-idb strat))


      ; TODO (here, elsewhere?): adding (re)discovered tuples immediately to a current `all-tuples` set (quicker convergence?)

      ; (printf "tuples removed glob: ~a\n" tuples-removed-glob)
      ; (printf "p->r-edb: ~a\n" p->r-edb)
      ; (printf "p->r-edb¬: ~a\n" p->r-edb¬)
      ; (printf "p->r-idb: ~a\n" p->r-idb)

      ; (define edb-rules*  COMMENTED OUT: no removed-glob: check neg-deps below if tuples-removed-glob required
      ;   (set-union
      ;     (select-rules-for-tuples real-delta-tuples-edb p->r-edb)
      ;     (select-rules-for-tuples tuples-removed-glob p->r-edb¬))) ; TODO: tuples-removed-glob is not scoped to stratum

      (define edb-rules* (select-rules-for-tuples real-delta-tuples-edb p->r-edb))

      (printf "selected EDB rules: ~a\n" edb-rules*)
      (printf "provenance\n") (print-map provenance)
      (define neg-deps (list->set (set-map real-delta-tuples-edb ¬)))
      (printf "neg deps ~a\n" neg-deps)

      ; remove tuples that depend on absence of tuples added in stratum below
      (define provenance* (remove-variables-from-system provenance neg-deps))
      (printf "provenance after removing neg deps\n") (print-map provenance*)
      
      ; UGH
      (define t (list->set (hash-keys provenance)))
      (define t* (list->set (hash-keys provenance*)))
      (define remmed (set-subtract t t*))
      (printf "removed due to neg dep: ~a\n" remmed)
      (set! tuples (set-subtract tuples remmed)) ; TODO ugh! integrate tuples and provenance!

      (let delta-rule-loop-edb ((edb-rules* edb-rules*) (delta-tuples (set)) (provenance provenance*))
        (if (set-empty? edb-rules*)
          
          (let ((real-delta-tuples-idb (set-subtract delta-tuples tuples))) ; TODO?: full tuples set subtr
            ;(printf "*real-delta-tuples-idb ~a\n" real-delta-tuples-idb)
            (let delta-loop-idb ((delta-rules (select-rules-for-tuples real-delta-tuples-idb p->r-idb)) (tuples (set-union tuples real-delta-tuples-edb real-delta-tuples-idb)) (previous-delta-tuples real-delta-tuples-idb) (provenance provenance) (tuples-added real-delta-tuples-idb))
              ;(printf "tuples: (~a) ~a\n" (set-count tuples) tuples)
              (let delta-rule-loop-idb ((idb-rules* delta-rules) (delta-tuples (set)) (provenance provenance))
                (if (set-empty? idb-rules*)
                  (let ((real-delta-tuples-idb (set-subtract delta-tuples tuples)))
                    ;(printf "real-delta-tuples-idb ~a\n" real-delta-tuples-idb)
                    (if (set-empty? real-delta-tuples-idb)
                        (values tuples provenance tuples-added)
                        (delta-loop-idb (select-rules-for-tuples real-delta-tuples-idb p->r-idb) (set-union tuples real-delta-tuples-idb) real-delta-tuples-idb provenance (set-union tuples-added real-delta-tuples-idb))))
                  (let ((rule (set-first idb-rules*)))
                    (let ((derived-tuples-with-provenance-for-rule (fire-rule rule tuples previous-delta-tuples)))
                      (printf "fired (idb) ~a got ~a\n" rule derived-tuples-with-provenance-for-rule)
                      (let-values (((derived-tuples-for-rule provenance)
                          (for/fold ((derived-tuples-for-rule (set)) (provenance provenance)) ((fr (in-set derived-tuples-with-provenance-for-rule)))
                            (match-let (((cons derived-tuple prov) fr))
                              (values (set-add derived-tuples-for-rule derived-tuple) (update-provenance provenance derived-tuple prov))))))
                        (set! num-derived-tuples (+ num-derived-tuples (set-count derived-tuples-for-rule)))
                        (delta-rule-loop-idb (set-rest idb-rules*) (set-union delta-tuples derived-tuples-for-rule) provenance))))))))
                      
            (let ((rule (set-first edb-rules*)))
              (let ((derived-tuples-with-provenance-for-rule (fire-rule rule tuples real-delta-tuples-edb)))
                (printf "fired (edb) ~a got ~a\n" rule derived-tuples-with-provenance-for-rule)
                (let-values (((derived-tuples-for-rule provenance)
                    (for/fold ((derived-tuples-for-rule (set)) (provenance provenance)) ((fr (in-set derived-tuples-with-provenance-for-rule)))
                      (match-let (((cons derived-tuple prov) fr))
                        (values (set-add derived-tuples-for-rule derived-tuple) (update-provenance provenance derived-tuple prov))))))
                  (set! num-derived-tuples (+ num-derived-tuples (set-count derived-tuples-for-rule)))
                  (delta-rule-loop-edb (set-rest edb-rules*) (set-union delta-tuples derived-tuples-for-rule) provenance)))
                  ))))

    (define (stratum-loop S E provenance tuples-add E0)
        ;(printf "\ni stratum ~a/~a with ~a tuples, tuples-add ~a\n" (- (set-count strata) (set-count S)) (set-count strata) (set-count E) tuples-add)
        ;(printf "provenance stratum\n") (print-map provenance) (newline)
        (if (null? S)
            (solver-result E num-derived-tuples (make-delta-solver E0 provenance)) ; TODO: redundant (set-subtract ...)
            (let-values (((E* provenance tuples-added) (stratum-rule-loop (car S) E tuples-add provenance)))
              (stratum-loop (cdr S) E* provenance tuples-added E0))))

    (let ((intersection (set-intersect tuples-add* tuples-remove*))) ; TODO: we first remove, and then add: check whether/how this matters!
      (let ((tuples-add (set-subtract tuples-add* intersection)))
        (let ((tuples-remove (set-subtract tuples-remove* intersection)))
          (printf "\n\nSOLVING INCREMENTAL-DELTA\nadd ~a\nremove ~a\n" tuples-add tuples-remove)
          ;(printf "old provenance\n") (print-map provenance) (newline)
          ; REMOVAL OF IDBs BASED ON + DEPS: use provenance
          ; REMOVAL OF IDBs BASED ON - DEPS: also use provenance (!)
          (let ((provenance* (remove-variables-from-system provenance (set-union tuples-remove tuples-add))))
            ;(printf "provenance after remove\n") (print-map provenance*) (newline)
            (let ((E0-removed (set-subtract E0 tuples-remove)))

                ; TODO: check performance impact of keeping separate (redundant) list of all tuples (iso. keeping them as union of keys)
                (stratum-loop (cdr strata) (set-union E0-removed (list->set (hash-keys provenance*))) provenance* tuples-add (set-union E0-removed tuples-add)))))))) ; end solve-i-delta

  (define (make-delta-solver E0 provenance) ; E0 are initial EDBs, prov keys are all derived IDBs
    ;(printf "MAKE-D-SOLVER\nEDB ~a\nIDB ~a\n" E0 (hash-keys provenance))
    (lambda (deltas) ; deltas are initial EDBs that are added/removed
      (let-values (((tuples-add tuples-remove)
        (for/fold ((tuples-add (set)) (tuples-remove (set))) ((delta (in-list deltas)))
          (match delta
            ((add-tuple tuple) (values (set-add tuples-add tuple) tuples-remove))
            ((remove-tuple tuple) (values tuples-add (set-add tuples-remove tuple)))))))
        (solve-incremental-delta E0 provenance tuples-add tuples-remove))))

  (solve-incremental-initial E))

(define (print-map m)
  (for (((key value) (in-hash m)))
    (printf "~a -> ~a\n" key value)))

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

; rewrites positive EDB preds in rules that contain them
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
                     ; don't rewrite: use all tuples (in principle EDB0 should suffice: no new tuples can be generated that would change neg pred)
                     ; but do add!
                (rewrite-terms (cons term previous-terms) (cdr future-terms) (set-add rewrites r)))
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
; out: (values, mapping pred -> rules with +pred in body, mapping pred -> rules with -pred in body) 
(define (pred-to-rules rules)
  (for/fold ((R (hash)) (R¬ (hash))) ((r (in-set rules)))
    (for/fold ((R R) (R¬ R¬)) ((term (in-list (rule-body r))))
      (match term
        ((¬ p)
          (let ((term-name (atom-name p)))
            (values R (hash-set R¬ term-name (set-add (hash-ref R¬ term-name (set)) r)))))
        (_ 
          (let ((term-name (atom-name term)))
            (if (equal? term-name '*Recent*)
                (let ((recent-term (vector-ref term 1)))
                  (let ((recent-name (atom-name recent-term)))
                    (values (hash-set R recent-name (set-add (hash-ref R recent-name (set)) r)) R¬)))
                (values (hash-set R term-name (set-add (hash-ref R term-name (set)) r)) R¬))))
        )))) 

; in: set of tuples and map of pred->rules
; out: all rules that need to be fired (based on the preds in rules' body)
(define (select-rules-for-tuples tuples p->r)
  (for/fold ((R (set))) ((tuple (in-set tuples)))
    (set-union R (hash-ref p->r (atom-name tuple) (set)))))

; selects rules that only contain EDB preds (including only negs)
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

(define (annotate-stratum stratum-rules)
  (printf "***** annotate stratum ~a\n" stratum-rules)
  (define edb-rules (get-edb-rules stratum-rules))
  (define semi-naive-rules-edb (rewrite-semi-naive-edb stratum-rules))
  (printf "semi-naive rules edb: ~a\n" semi-naive-rules-edb)
  (define semi-naive-rules-idb (rewrite-semi-naive-idb stratum-rules))
  (printf "semi-naive rules idb: ~a\n" semi-naive-rules-idb)
  (define-values (p->r-idb p->r-idb¬) (pred-to-rules semi-naive-rules-idb))
  (define-values (p->r-edb p->r-edb¬) (pred-to-rules semi-naive-rules-edb))
  (printf "p->r-edb ~a\n" p->r-edb)
  (printf "p->r-edb¬ ~a\n" p->r-edb¬)
  (printf "p->r-idb ~a\n" p->r-idb)
  (printf "p->r-idb¬ ~a\n" p->r-idb¬)

  (stratum edb-rules p->r-edb p->r-edb¬ p->r-idb))
