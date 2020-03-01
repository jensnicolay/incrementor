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
  (solve-incremental-initial strata E))

(define (solve-incremental-initial strata tuples)
  (define-values (tuples* provenance* num-derived-tuples) (stratum-loop-initial strata tuples (hash) 0))
  (solver-result tuples* num-derived-tuples (make-delta-solver strata tuples* provenance*)))

(define (stratum-loop-initial S E provenance num-derived-tuples)
  ; (printf "\nincr initial stratum ~a with ~a tuples\n" (set-count S) (set-count E))
  (if (null? S)
      (values E provenance num-derived-tuples); E0 are the initial EDBs, prov keys are all derived IDBs
      (let ((stratum (car S)))
        (define-values (tuples* provenance* num-derived-tuples*) (stratum-rule-loop-initial stratum E provenance))
        (stratum-loop-initial (cdr S) tuples* provenance* (+ num-derived-tuples num-derived-tuples*)))))

(define (stratum-rule-loop-initial stratum tuples provenance) ; per stratum, initial

  (define edb-rules (stratum-edb-rules stratum))
  (define num-derived-tuples 0)
  (define p->r (stratum-p->r-idb stratum))

  (let loop ((delta-rules edb-rules) (tuples tuples) (previous-delta-tuples (set)) (provenance provenance))
    ; (printf "delta rules :~a\n" delta-rules)
    ; (printf "tuples :~a\n" tuples)
    ; (printf "previous delta tuples :~a\n" previous-delta-tuples)
    (define-values (real-delta-tuples* provenance* num-derived-tuples*) (delta-rule-loop-idb delta-rules previous-delta-tuples tuples provenance))
    (if (set-empty? real-delta-tuples*)
        (values tuples provenance* num-derived-tuples*) 
        (loop (select-rules-for-tuples real-delta-tuples* p->r) (set-union tuples real-delta-tuples*) real-delta-tuples* provenance*))))

(define (solve-incremental-delta strata tuples provenance tuples-add* tuples-remove*) ; prov keys are onlhy IDB!, tuples-add/remove are initial delta EDBs
  (define intersection (set-intersect tuples-add* tuples-remove*)) ; TODO: we first remove, and then add: check whether/how this matters!
  (define tuples-add (set-subtract tuples-add* intersection))
  (define tuples-remove (set-subtract tuples-remove* intersection))
  ; (printf "\n\nSOLVING INCREMENTAL-DELTA\nadd ~a\nremove ~a\n" tuples-add tuples-remove)

  ;; idb tuple removal due to removal of edb tuples with pos deps
  ;(printf "provenance before remming ~a\n" tuples-remove) (print-map provenance) (newline)
  (define provenance* (remove-variables-from-system provenance tuples-remove))
  (define t (list->set (hash-keys provenance)))
  (define t* (list->set (hash-keys provenance*))) ; all idbs
  (define remmed (set-subtract t t*))
  ; (printf "tuples removed due to edb tuple removal: ~a\n" remmed)
  (define tuples* (set-subtract (set-union tuples tuples-add) (set-union tuples-remove remmed)))

  ;(define E0-removed (set-subtract E0 tuples-remove))
  (define-values (tuples** provenance** num-derived-tuples) (stratum-loop-delta strata tuples* provenance* tuples-add remmed 0))
  (solver-result tuples** num-derived-tuples (make-delta-solver strata tuples** provenance**))) ; TODO: redundant (set-subtract ...)

(define (stratum-loop-delta S E provenance tuples-added tuples-removed-glob num-derived-tuples)
  ; (printf "\nincr delta stratum ~a with ~a tuples, tuples-add ~a\n" (set-count S) (set-count E) tuples-added)
  (if (null? S)
      (values E provenance num-derived-tuples) ; TODO: redundant (set-subtract ...)
      (let ((stratum (car S)))
        (define-values (tuples* provenance* tuples-added* num-derived-tuples*) (stratum-rule-loop-delta stratum E tuples-added tuples-removed-glob provenance))
        (stratum-loop-delta (cdr S) tuples* provenance* tuples-added* tuples-removed-glob (+ num-derived-tuples num-derived-tuples*)))))

(define (stratum-rule-loop-delta stratum tuples delta-edb-tuples tuples-removed-glob provenance) ; per stratum, incremental
  (define-values (delta-idb-tuples tuples* provenance* num-tuples-derived) (process-edb-delta stratum delta-edb-tuples tuples-removed-glob tuples provenance))
  (process-idb-delta stratum delta-idb-tuples tuples* provenance* num-tuples-derived))

(define (make-delta-solver strata tuples provenance)
  (lambda (deltas) ; deltas are initial EDBs that are added/removed
    (let-values (((tuples-add tuples-remove)
      (for/fold ((tuples-add (set)) (tuples-remove (set))) ((delta (in-list deltas)))
        (match delta
          ((add-tuple tuple) (values (set-add tuples-add tuple) tuples-remove))
          ((remove-tuple tuple) (values tuples-add (set-add tuples-remove tuple)))))))
      (solve-incremental-delta strata tuples provenance tuples-add tuples-remove))))

(define (process-edb-delta strat tuples-added tuples-removed-glob tuples provenance)

  (define p->r-edb (stratum-p->r-edb strat))
  (define p->r-edb¬ (stratum-p->r-edb¬ strat))
  (define num-derived-tuples 0)

  ;; idb tuple removal due to addition of edb tuples with neg deps
  (define neg-deps (list->set (set-map tuples-added ¬)))
  ; (printf "provenance before remming ¬~a\n" tuples-added) (print-map provenance) (newline)
  (define provenance* (remove-variables-from-system provenance neg-deps))
  ; UGH
  (define t (list->set (hash-keys provenance)))
  (define t* (list->set (hash-keys provenance*)))
  (define remmed (set-subtract t t*))
  ; (printf "tuples removed due to edb tuple addition: ~a\n" remmed)
  (define tuples* (set-subtract tuples remmed)) ; TODO ugh! integrate tuples and provenance!

  ;; idb tuple addition due to removal of edb tuples with neg deps
  (define rules-neg (select-rules-for-tuples tuples-removed-glob p->r-edb¬))
  ; (printf "rules for adding idb tuples due to edb tuple removal: ~a\n" rules-neg)
  (define-values (delta-tuples-neg provenance** num-derived-tuples*) (delta-rule-loop-edb rules-neg tuples-removed-glob tuples* provenance*))
  (set! num-derived-tuples (+ num-derived-tuples num-derived-tuples*))
  ; (printf "tuples added due to edb tuple removal: ~a\n" delta-tuples-neg)

  ;; idb tuple addition due to addition of edb tuples with pos deps
  (define rules-pos (select-rules-for-tuples tuples-added p->r-edb))
  ; (printf "rules for adding idb tuples due to edb tuple addition: ~a\n" rules-pos)
  (define-values (delta-tuples-pos provenance*** num-derived-tuples**) (delta-rule-loop-edb rules-pos tuples-added tuples* provenance**))
  (set! num-derived-tuples (+ num-derived-tuples num-derived-tuples**))
  ; (printf "tuples added due to edb tuple addition: ~a\n" delta-tuples-pos) ; TODO: seems too large sometimes (e.g. Node 3 when removing Link 2 3)
  (define tuples** (set-union tuples* tuples-added))
  (define delta-idb-tuples (set-subtract (set-union delta-tuples-neg delta-tuples-pos) tuples**)) ; TODO?: full tuples set subtr
  (values delta-idb-tuples tuples** provenance*** num-derived-tuples))

(define (delta-rule-loop-edb rules recent-tuples tuples provenance)
  (let loop ((rules rules) (delta-tuples (set)) (provenance provenance) (num-derived-tuples 0))
    (if (set-empty? rules)
        (values delta-tuples provenance num-derived-tuples)
        (let ((rule (set-first rules)))
          (let ((derived-tuples-with-provenance-for-rule (fire-rule rule tuples recent-tuples)))
            ; (printf "fired (edb) ~a got ~a\n" rule derived-tuples-with-provenance-for-rule)
            (let-values (((derived-tuples-for-rule provenance*)
                (for/fold ((derived-tuples-for-rule (set)) (provenance provenance)) ((fr (in-set derived-tuples-with-provenance-for-rule)))
                  (match-let (((cons derived-tuple prov) fr))
                    (values (set-add derived-tuples-for-rule derived-tuple) (update-provenance provenance derived-tuple prov))))))
              (loop (set-rest rules) (set-union delta-tuples derived-tuples-for-rule) provenance* (+ num-derived-tuples (set-count derived-tuples-for-rule)))))))))

(define (process-idb-delta strat delta-idb-tuples tuples provenance num-derived-tuples)
  (define p->r-idb (stratum-p->r-idb strat))
  (define num-derived-tuples 0)
  (let loop ((delta-rules (select-rules-for-tuples delta-idb-tuples p->r-idb)) (tuples (set-union tuples delta-idb-tuples)) (previous-delta-tuples delta-idb-tuples) (tuples-added delta-idb-tuples) (provenance provenance))      
      (define-values (real-delta-tuples-idb* provenance* num-derived-tuples*) (delta-rule-loop-idb delta-rules previous-delta-tuples tuples provenance))
      (set! num-derived-tuples (+ num-derived-tuples num-derived-tuples*))
      (if (set-empty? real-delta-tuples-idb*)
          (values tuples provenance* tuples-added num-derived-tuples)
          (let ((delta-rules* (select-rules-for-tuples real-delta-tuples-idb* p->r-idb)))
            (loop delta-rules* (set-union tuples real-delta-tuples-idb*) real-delta-tuples-idb* (set-union tuples-added real-delta-tuples-idb*) provenance*)))))

(define (delta-rule-loop-idb delta-rules previous-delta-tuples tuples provenance) ; initial and delta
  (let loop ((idb-rules* delta-rules) (delta-tuples (set)) (provenance provenance) (num-derived-tuples 0))
    (if (set-empty? idb-rules*)
        (let ((real-delta-tuples-idb (set-subtract delta-tuples tuples))) ; TODO: subtract with full tuples here
        ;(printf "real-delta-tuples-idb ~a\n" real-delta-tuples-idb)
          (values real-delta-tuples-idb provenance num-derived-tuples)) ;delta-loop-idb
        (let ((rule (set-first idb-rules*)))
          (let ((derived-tuples-with-provenance-for-rule (fire-rule rule tuples previous-delta-tuples)))
            ; (printf "fired ~a got ~a\n" rule derived-tuples-with-provenance-for-rule)
            (let-values (((derived-tuples-for-rule provenance*)
                (for/fold ((derived-tuples-for-rule (set)) (provenance provenance)) ((fr (in-set derived-tuples-with-provenance-for-rule)))
                  (match-let (((cons derived-tuple prov) fr))
                    (values (set-add derived-tuples-for-rule derived-tuple) (update-provenance provenance derived-tuple prov))))))
              (loop (set-rest idb-rules*) (set-union delta-tuples derived-tuples-for-rule) provenance* (+ num-derived-tuples (set-count derived-tuples-for-rule)))))))))

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
      
    (define (rewrite-terms previous-terms future-terms rewrites-pos rewrites-neg)
      (if (null? future-terms)
          (values rewrites-pos rewrites-neg) ; rules without EDB preds are filtered out
          (let ((term (car future-terms)))
            (match term
              ((¬ p) ; in a stratified Datalog, all negated preds are guaranteed to be EDBs
                (rewrite-terms (cons term previous-terms) (cdr future-terms)
                  ; (set-add rewrites-pos r) ; don't rewrite: use all tuples (in principle EDB0 should suffice: no new tuples can be generated that would change neg pred), but do add!
                  rewrites-pos
;                  (set-add rewrites-neg (rule head (append (reverse previous-terms) (list (¬ `#(*Recent* ,p))) (cdr future-terms)))))) TODO ;specific firing of rules with neg dep on removed edb tuple
                  (set-add rewrites-neg (rule head (append (reverse previous-terms) (list (¬ p)) (cdr future-terms))))))
              (_
                (let ((name (atom-name term)))
                  (if (set-member? idb-preds name)
                      (rewrite-terms (cons term previous-terms) (cdr future-terms) 
                        rewrites-pos 
                        rewrites-neg)
                      (rewrite-terms (cons term previous-terms) (cdr future-terms) 
                        (set-add rewrites-pos (rule head (append (reverse previous-terms) (list `#(*Recent* ,term)) (cdr future-terms))))
                        rewrites-neg))))))))

    ; (define (rewrite-term-pos term previous-terms future-terms)
    ;   (cons head (append previous-terms (list `#(*Recent* ,term)) future-terms)))
    
    (rewrite-terms '() body (set) (set))))

    (for/fold ((rewritten-rules-pos (set)) (rewritten-rules-neg (set))) ((r (in-set rules)))
      (let-values (((rewrites-pos rewrites-neg) (rewrite-rule r)))
        (values (set-union rewritten-rules-pos rewrites-pos) (set-union rewritten-rules-neg rewrites-neg))))))

; in: rules
; out: (values, mapping pred -> rules with +pred in body, mapping pred -> rules with -pred in body) 
(define (pred-to-rules rules)
  (for/fold ((R (hash))) ((r (in-set rules)))
    (for/fold ((R R)) ((term (in-list (rule-body r))))
      (match term
        ((¬ p)
          (let ((term-name (atom-name p)))
            (if (equal? term-name '*Recent*)
                (let ((recent-term (vector-ref p 1)))
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
  ; (printf "***** annotate stratum ~a\n" stratum-rules)
  (define edb-rules (get-edb-rules stratum-rules))

  (define-values (semi-naive-rules-edb-pos semi-naive-rules-edb-neg) (rewrite-semi-naive-edb stratum-rules))
  ; (printf "semi-naive rules edb pos: ~a\n" semi-naive-rules-edb-pos)
  ; (printf "semi-naive rules edb neg: ~a\n" semi-naive-rules-edb-neg)
  
  (define semi-naive-rules-idb (rewrite-semi-naive-idb stratum-rules))
  ; (printf "semi-naive rules idb: ~a\n" semi-naive-rules-idb)

  (define p->r-edb (pred-to-rules semi-naive-rules-edb-pos))
  (define p->r-edb¬ (pred-to-rules semi-naive-rules-edb-neg))
  (define p->r-idb (pred-to-rules semi-naive-rules-idb))
  ; (printf "p->r-edb ~a\n" p->r-edb)
  ; (printf "p->r-edb¬ ~a\n" p->r-edb¬)
  ; (printf "p->r-idb ~a\n" p->r-idb)
  ; (printf "p->r-idb¬ ~a\n" p->r-idb¬)

  (stratum edb-rules p->r-edb p->r-edb¬ p->r-idb))







(define r1 (:- #(Reachable x y)   #(Link x y)))
(define r2 (:- #(Reachable x y)   #(Link x z) #(Reachable z y)))
(define r3 (:- #(Node x)          #(Link x y)))
(define r4 (:- #(Node y)          #(Link x y)))
(define r5 (:- #(Unreachable x y) #(Node x) #(Node y) (¬ #(Reachable x y))))

(define P (set r1 r2 r3 r4 r5))
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

(for/fold ((solver (solve-incremental P E))) ((delta deltas))
  (define solver* (solver-result-delta-solver solver))
  (solver* (list delta)))
