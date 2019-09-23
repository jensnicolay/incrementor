#lang racket

(provide (all-defined-out))

(random-seed 111)
(define ns (make-base-namespace))

(struct ¬ (p) #:transparent)
(struct ← (p) #:transparent)

(define (atom-name a)
  (vector-ref a 0))

(define (atom-arity a)
  (sub1 (vector-length a)))

(define (atom-term a i)
  (vector-ref a (add1 i)))

(define (--> head . body)
  (cons head body))
  
(define (rule-head r)
  (car r))

(define (rule-body r)
  (cdr r))

(struct ledge (to label) #:transparent)

(define (lsuccessors G n)
  (for/fold ((R (set))) ((s (in-set (hash-ref G n (set)))))
    (set-add R (ledge-to s))))

(define (ltranspose G)
  (for/fold ((G* (hash))) (((from tos) (in-hash G)))
    (for/fold ((G* (hash-set G* from (hash-ref G* from (set))))) ((to (in-set tos)))
      (match-let (((ledge v label) to))
        (hash-set G* v (set-add (hash-ref G* v (set)) (ledge from label)))))))

(define (sinks G)
  (for/set (((from tos) (in-hash G)) #:when (set-empty? tos))
    from))

(define (precedence-lgraph P)
  (for/fold ((G (hash))) ((r (in-set P)))
    (let ((head (atom-name (car r))))
      (for/fold ((G (hash-set G head (hash-ref G head (set))))) ((p (in-list (cdr r))))
        (match p
          ((¬ n)
            (let ((dep (atom-name n)))
              (hash-set G dep (set-add (hash-ref G dep (set)) (ledge head ¬)))))
          (_
            (let ((dep (atom-name p)))
              (hash-set G dep (set-add (hash-ref G dep (set)) (ledge head +)))))
        )))))

; Tarjan
(define (lscc G)

  (define index 0)
  (define S '())
  (define Index (hash))
  (define Lowlink (hash))
  (define Onstack (hash))
  (define SCC (set))

  (define (strongconnect v)
    (set! Index (hash-set Index v index))
    (set! Lowlink (hash-set Lowlink v index))
    (set! index (add1 index))
    (set! S (cons v S))
    (set! Onstack (hash-set Onstack v #t))

    (for ((w (in-set (lsuccessors G v))))
      (if (not (hash-ref Index w #f))
          (begin
            (strongconnect w)
            (set! Lowlink (hash-set Lowlink v (min (hash-ref Lowlink v) (hash-ref Lowlink w)))))
          (when (hash-ref Onstack w)
              (set! Lowlink (hash-set Lowlink v (min (hash-ref Lowlink v) (hash-ref Index w)))))))

    (when (= (hash-ref Lowlink v) (hash-ref Index v))
      (letrec ((f (lambda (scc)
                    (let ((w (car S)))
                      (set! S (cdr S))
                      (set! Onstack (hash-set Onstack w #f))
                      ;(printf "~v ~v\n" (hash-ref Index v) w)
                      (set! scc (cons w scc))
                      (if (not (equal? w v))
                          (f scc)
                          (set! SCC (set-add SCC scc)))))))
        (f '()))))

  (for ((v (in-list (hash-keys G))))
    (let ((index (hash-ref Index v #f)))
      (unless index (strongconnect v))))

  SCC)

(define (scc-map SCC) ; TODO this can be folded into SCC 
  (let loop ((index 0) (SCC SCC) (R (hash)))
    (if (set-empty? SCC)
        R
        (let ((C (set-first SCC)))
          (let ((R*
              (for/fold ((R R)) ((v (in-list C)))
                (hash-set R v index))))
            (loop (add1 index) (set-rest SCC) R*))))))


(define (topo-sort G)

  (define V (set))
  (define R '())

  (define (dfs v)
    (set! V (set-add V v))
    (for ((s (in-set (hash-ref G v))))
      (unless (set-member? V s)
        (dfs s)))
    (set! R (cons v R)))

  (for ((v (in-list (hash-keys G))))
      (unless (set-member? V v)
        (dfs v)))

  R)


(define (strata P)

  (define G-pred (precedence-lgraph P))
  (define SCC (lscc G-pred))
  (define v2cid (scc-map SCC))

  (define G-red
    (for/fold ((R (hash))) (((from edges) (in-hash G-pred)))
      (let ((from-cid (hash-ref v2cid from)))
        (for/fold ((R (hash-set R from-cid (hash-ref R from-cid (set))))) ((edge (in-set edges)))
          (hash-set R from-cid (set-add (hash-ref R from-cid) (hash-ref v2cid (ledge-to edge))))))))

  (define cid-sorted (topo-sort G-red))
  (printf "topo: ~v\n" cid-sorted)

  (define cid2C (for/fold ((R (hash))) (((v cid) (in-hash v2cid)))
                        (hash-set R cid (set-add (hash-ref R cid (set)) v))))
  (printf "cid2C: ~v\n" cid2C)

  ; (define Strata (for/fold ((R (hash))) ((cid (in-list cid-sorted)))
  ;                   (for/fold ((R R)) ((v (in-set (hash-ref cid2C cid))))
  ;                     (hash-set R v cid))))

  (define Strata (map (lambda (cid) (hash-ref cid2C cid)) cid-sorted))

  Strata)

(define (stratify P)
  (define S (strata P))
  (printf "strata: ~v\n" S)

  (map (lambda (Preds)
          (for/fold ((R (set))) ((Pred (in-set Preds)))
            (set-union R (for/set ((r (in-set P)) #:when (eq? (atom-name (car r)) Pred)) ; why not lists for "set" of rules?
                            r))))
        S))

(define (evalare x ρ)
  (match x
    ((cons 'unquote y)
      (evalare2 (car y) ρ))
    (_
      x)))

(define (evalare2 x ρ)
  ;(printf "evalare ~a\n" x)
  (cond
    ((symbol? x)
      (hash-ref ρ x (lambda () (eval x ns))))
    ((list? x)
      (let ((rator (evalare2 (car x) ρ)))
        (let ((rands (map (lambda (rand) (evalare2 rand ρ)) (cdr x))))
          (apply rator rands))))
   (else x)))


(define (matchare xxx y ρ) ; x = rule, y = fact
  ;(printf "matchare ~a ~a ~a\n" xxx y ρ)
  (let ((x (evalare xxx ρ)))
    (cond
      ((and (vector? x) (vector? y) (eq? (vector-ref x 0) (vector-ref y 0)) (= (atom-arity x) (atom-arity y)))
        (let term-loop ((i 1) (ρ ρ))
          (if (= i (vector-length x))
              ρ
              (let ((xx (vector-ref x i)))
                (let ((yy (vector-ref y i)))
                  (let ((ρ* (matchare xx yy ρ)))
                    (if ρ*
                        (term-loop (add1 i) ρ*)
                        #f)))))))
      ((eq? x '_)
        ρ)
      ((symbol? x)
        (if (hash-has-key? ρ x)
              (let ((existing-value (hash-ref ρ x)))
                  (if (equal? existing-value y)
                      ρ
                      #f))
              (hash-set ρ x y)))
      ((and (pair? x) (eq? (car x) 'quote))
        (if (eq? (cadr x) y)
            ρ
            #f))
      ((equal? x y)
        ρ)
      (else #f))))

(define (bind-fact hv env)                     
  (let ((terms 
    (for/list ((i (in-range (atom-arity hv))))
      (let ((x (atom-term hv i)))
        (cond
          ((symbol? x)
            (hash-ref env x (lambda () (error 'bind-fact "no value for ~a in ~a" x env))))
          ((vector? x)
            (bind-fact x env))
          ((and (pair? x) (eq? (car x) 'quote))
            (cadr x))
          (else x))))))
  (let ((new-fact (apply vector-immutable (cons (atom-name hv) terms))))
    new-fact)))

(define (fire rule E delta-tuples)
  ;(printf "fire rule ~v E ~v\n" rule E)
  (let loop ((W (set (cons (cdr rule) (hash)))) (ΔE (set)))
    ;(printf "loop ~v\n" W)
    (if (set-empty? W)
        ΔE
        (match-let (((cons atoms env) (set-first W)))
        ;(printf "looking at atoms ~v in ~v\n" atoms env)
        (if (null? atoms)
            (let ((hv (car rule)))
              (let ((new-fact (bind-fact hv env)))
                (loop (set-rest W) (set-add ΔE new-fact))))
            (let ((av (car atoms)))
              (match av
                ((¬ av) ; duplicating special terms, other strategy: let special forms return results one by one for "fail-fast"
                  (match av
                    ((vector '= p q)
                      (let ((atoms-rest (cdr atoms)))
                        (let ((pp (evalare p env)))
                          (let ((qq (evalare q env)))
                            (let ((env* (matchare pp qq env)))
                              (if env*
                                  (loop (set-rest W) ΔE)
                                  (loop (set-add (set-rest W) (cons (cdr atoms) env)) ΔE)))))))
                    (_ ; TODO: *Recent* for neg: possible?
                      (let e-loop ((E E))
                        (if (set-empty? E)
                            (loop (set-add (set-rest W) (cons (cdr atoms) env)) ΔE)
                            (let ((ev (set-first E)))
                              (let ((env* (matchare av ev env)))
                                ;(printf "¬ matchare result ~a ~a: ~v\n" av ev m)
                                (if env*
                                    (loop (set-rest W) ΔE)
                                    (e-loop (set-rest E))))))))))
                ((vector 'DEBUG name)
                  (printf "~a: about to match ~a with ~a\n\n" name (cadr atoms) env)
                  (loop (set-add (set-rest W) (cons (cdr atoms) env)) ΔE))
                ((vector '∈1 x index l) ; for all (x, i) in l
                  (let ((d-lst (evalare2 l env)))
                    (let ((atoms-rest (cdr atoms)))
                      (loop (for/fold ((W (set-rest W))) ((el d-lst) (i (in-naturals)))
                                          (set-add W (cons atoms-rest (hash-set (hash-set env index i) x el))))
                                        ΔE))))
                ((vector '∈3 x index l) ; select x at index in l
                  (let ((d-index (evalare2 index env)))
                    (let ((d-lst (evalare2 l env)))
                      (let ((atoms-rest (cdr atoms)))
                          (if (or (null? d-lst) (>= d-index (length d-lst)))
                              (loop (set-rest W) ΔE)
                              (loop (set-add (set-rest W) (cons atoms-rest (hash-set env x (list-ref d-lst d-index)))) ΔE))))))
                ((vector '∈5 x index l) ; index of x in l
                  (let ((d-x (evalare2 index env)))
                    (let ((d-lst (evalare2 l env)))
                      (let ((atoms-rest (cdr atoms)))
                        (loop (set-add (set-rest W) (cons atoms-rest (hash-set env index (index-of d-lst d-x)))) ΔE))))) ; only finds first index
                ((vector '∈1 x l) ; for all x in l
                  (let ((d-lst (evalare2 l env)))
                    (let ((atoms-rest (cdr atoms)))
                      (if (null? d-lst)
                          (loop (set-rest W) ΔE)
                          (loop (for/fold ((W (set-rest W))) ((el d-lst))
                            (set-add W (cons atoms-rest (hash-set env x el))))
                            ΔE)))))
                ((vector '= p q)
                  (let ((atoms-rest (cdr atoms)))
                    (let ((pp (evalare p env)))
                      (let ((qq (evalare q env)))
                        (let ((env* (matchare pp qq env)))
                        (if env*
                            (loop (set-add (set-rest W) (cons atoms-rest env*)) ΔE)
                            (loop (set-rest W) ΔE)))))))
                ((vector '*Recent* p)
                  (let ((W (match-predicate p (cdr atoms) delta-tuples env (set-rest W))))
                    (loop W ΔE)))
                (_ 
                  (let ((W (match-predicate av (cdr atoms) E env (set-rest W))))
                    (loop W ΔE)))
                  )))))))

(define (match-predicate av remaining-preds E env W)
  (let e-loop ((E E) (W W))
    (if (set-empty? E)
        W
        (let ((ev (set-first E)))
          (let ((env* (matchare av ev env)))
            ;(when m (printf "matchare result: ~v\n" m))
            (if env*
                (e-loop (set-rest E) (set-add W (cons remaining-preds env*)))
                (e-loop (set-rest E) W)))))))

(define (solve-naive P E)

  (define S* (stratify P))
  ;(printf "stratify ~v\n\n" S*)

  (let inter-loop ((E E) (S S*))
    (printf "\ninter ~a/~a with ~a facts\n" (- (set-count S*) (set-count S)) (set-count S*) (set-count E))
    (if (null? S)
        E
        (let ((Pi (car S)))
          (printf "Pi: ~v\n" (list->set (set-map Pi (lambda (r) (atom-name (car r))))))
          (let intra-loop ((E-intra E))
            (let ((tuples (solve-naive-helper Pi E-intra)))
              (let ((E-intra* (set-union E-intra tuples)))
                  (if (equal? E-intra E-intra*) ; monotonicity: size check quicker? (TODO)
                      (inter-loop E-intra* (cdr S))
                      (let ((new-facts (set-subtract E-intra* E-intra)))
                        (printf "new facts: ~a\n" new-facts)
                        (intra-loop E-intra*))))))))))


(define (solve-naive-helper rules tuples)
  (let loop ((rules rules) (derived-tuples tuples))
    (if (set-empty? rules) 
        derived-tuples
        (let ((rule (set-first rules)))
          (let ((derived-tuples-for-rule (fire rule derived-tuples (set))))
            (loop (set-rest rules) (set-union derived-tuples derived-tuples-for-rule)))))))


(define (solve-semi-naive P E)

  (define S* (stratify P))

  (let stratum-loop ((S S*) (E E))
    (printf "\ninter ~a/~a with ~a facts\n" (- (set-count S*) (set-count S)) (set-count S*) (set-count E))

    (if (null? S)
        E
        (let ((E* (perform-iter (car S) E)))
          (stratum-loop (cdr S) E*)))))

(define (rewrite-semi-naive rules)
  (let ((idb-preds (for/set ((rule (in-set rules)))
                      (atom-name (rule-head rule))))) ; this corresponds to Strata?

  (define (rewrite-rule rule)
    (let ((head (rule-head rule))
          (body (rule-body rule)))
      
    (define (rewrite-terms previous-terms future-terms rewrites)
      (if (null? future-terms)
          (if (set-empty? rewrites)
              (set rule)
              rewrites)
          (let ((term (car future-terms)))
            (if (struct? term)
                (rewrite-terms (cons term previous-terms) (cdr future-terms) rewrites)
                (let ((name (atom-name term)))
                  (if (set-member? idb-preds name)
                      (rewrite-terms (cons term previous-terms) (cdr future-terms) (set-add rewrites (rewrite-term term (reverse previous-terms) (cdr future-terms))))
                      (rewrite-terms (cons term previous-terms) (cdr future-terms) rewrites)))))))

    (define (rewrite-term term previous-terms future-terms)
      (cons head (append previous-terms (list `#(*Recent* ,term)) future-terms)))
    
    (rewrite-terms '() body (set))))

    (for/fold ((rewritten-rules (set))) ((rule (in-set rules)))
        (set-union rewritten-rules (rewrite-rule rule)))))


(define (perform-iter rules tuples)

  (define semi-naive-rules (rewrite-semi-naive rules))
  (printf "semi-naive rules: ~a\ntuples: ~a\n" semi-naive-rules tuples)
  (define pred-name-to-rules
    (for/fold ((R (hash))) ((snr (in-set semi-naive-rules)))
      (for/fold ((R R)) ((term (in-list (rule-body snr))))
        (let ((term-name (atom-name term)))
          (if (equal? term-name '*Recent*)
              (let ((recent-term (vector-ref term 1)))
                (let ((recent-name (atom-name recent-term)))
                  (hash-set R recent-name (set-add (hash-ref R recent-name (set)) snr))))
              (hash-set R term-name (set-add (hash-ref R term-name (set)) snr))))))) ; always an EDB?
  (printf "pred-name-to-rules ~a\n" pred-name-to-rules)
  
  (let rule-loop ((rules* semi-naive-rules) (tuples tuples) (previous-delta-tuples tuples) (delta-tuples (set)))
    (let ((delta-rules
            (for/fold ((R (set))) ((previous-delta-tuple (in-set previous-delta-tuples)))
              (set-union R (hash-ref pred-name-to-rules (atom-name previous-delta-tuple) (set))))))
      (printf "delta rules :~a\n" delta-rules)
      (let delta-rule-loop ((rules* delta-rules) (delta-tuples delta-tuples))
        (if (set-empty? rules*)
          (let ((real-delta-tuples (set-subtract delta-tuples tuples)))
            (printf "delta-tuples ~a\nreal-delta-tuples ~a\n" delta-tuples real-delta-tuples)
            (if (set-empty? real-delta-tuples)
                tuples
                (rule-loop semi-naive-rules (set-union tuples real-delta-tuples) real-delta-tuples (set))))
          (let ((rule (set-first rules*)))
            (let ((derived-tuples-for-rule (fire rule tuples previous-delta-tuples)))
              ;(printf "fired ~a got ~a\n" rule derived-tuples-for-rule)
              (delta-rule-loop (set-rest rules*) (set-union delta-tuples derived-tuples-for-rule)))))))))


(module+ main
  123
)
