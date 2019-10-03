#lang racket

; Terminology
; * Term: Constant or Variable
; * Atom: Predicate + list of terms
; * EDB predicate: predicate in the source tables of the database
; * IDB predicate: predicate in tables derived by Datalog program

; Conventions
; * A prefix `l` indicates labelling.

; Naming
; * P   the set of rules
; * E   the set of facts

(provide (all-defined-out))

(random-seed 111)
(define ns (make-base-namespace))

(struct ¬ (p) #:transparent)
(struct rule (head body) #:transparent)

; An atom is structured as follows: Vect{ name | arg1 | arg2 | ... | argn }.
; A 'tuple' is a ground atom, i.e., an atom with only constants as args

(define (atom-name a)
  (vector-ref a 0))

(define atom? vector?)

(define (atom-arity a)
  (sub1 (vector-length a)))

(define (atom-term a i)
  (vector-ref a (add1 i)))

; A rule consists out of a head and a body, the latter again consisting out of an arbitrary number of atoms.

(define (:- head . body)
  (rule head body))

(define variable? symbol?)
  
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
    (let ((head (atom-name (rule-head r))))
      (for/fold ((G (hash-set G head (hash-ref G head (set))))) ((p (in-list (rule-body r))))
        (match p
          ((¬ n)
            (let ((dep (atom-name n)))
              (hash-set G dep (set-add (hash-ref G dep (set)) (ledge head ¬)))))
          (_
            (let ((dep (atom-name p)))
              (hash-set G dep (set-add (hash-ref G dep (set)) (ledge head +)))))
        )))))

(define (lscc-map G)

  ; Part 1: Tarjan's algorithm to find strongly connected components in a directed graph (= each node is reachable from every other node).

  (define index 0)
  (define S '())
  (define Index (hash))
  (define Lowlink (hash))
  (define Onstack (hash))
  (define SCC (set))        ; Strongly connected components.

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

  ; Part 2: Perform a mapping step. 

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

  (define G-pred (precedence-lgraph P))   ; Compute a precedence graph based on dependencies between the predicates.
  (define v2cid (lscc-map G-pred))        ; Determine strongly connected components.

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

  (map (lambda (cid) (hash-ref cid2C cid)) cid-sorted))

(define (stratify P)
  (define S (strata P))
  (printf "strata: ~v\n" S)

  (map (lambda (Preds)
          (for/fold ((R (set))) ((Pred (in-set Preds)))
            (set-union R (for/set ((r (in-set P)) #:when (eq? (atom-name (rule-head r)) Pred)) ; why not lists for "set" of rules?
                            r))))
        S))

; Evaluate unquoted eredpressions in a given environment. Eredpressions that are not unquoted are not evaluated.
(define (evaluate-unquoted red env)
  (match red
    ((cons 'unquote y) (evaluate (car y) env))
    (_ red)))

; Evaluate red in a given environment of bindings.
(define (evaluate red env)
  ;(printf "evaluate-unquoted ~a\n" red)
  (cond
    ((symbol? red) (hash-ref env red (lambda () (eval red ns)))) ; Look up symbols in the given environment.
    ((list? red)
      (let ((operator (evaluate (car red) env))
            (operands (map (lambda (operand) (evaluate operand env)) (cdr red))))
        (apply operator operands)))
    (else red)))

; Tries to unify an atom to a given pattern (also an atom). Requires atom and pattern to be atoms. Returns an eredtended environment or #f if unification fails.
(define (unify-atoms atom pattern env) ; TODO which is the pattern?
  (if (and (eq? (atom-name atom) (atom-name pattern)) (= (atom-arity atom) (atom-arity pattern)))
      (let unify-atom-arguments ((i 0) (env env)) ; Check unifiability for all argument terms of the atom.
        (if (= i (atom-arity pattern))
            env
            (let* ((atom-arg-i (atom-term atom i))
                   (patt-arg-i (atom-term pattern i))
                   (env* (unify-terms atom-arg-i patt-arg-i env))) ; Try to unify the i-th argument of both atoms.
              (if env*
                  (unify-atom-arguments (add1 i) env*)
                  #f)))) ; Unification of the i-th argument failed.
      #f)) ; The pattern has a different name or arity.

; Tries to unify a term to a given pattern. The pattern must be grounded but may contain wildcards (_). Returns an eredtended environment or #f if unification fails.
(define (unify-terms term pattern env) ; pattern must be grounded
  ;(printf "unify ~a with ~a in ~a\n" termred y env)
  (let ((red (evaluate-unquoted term env))) ; Reduce the term if needed.
    (cond
      ((and (atom? red) (atom? pattern)) (unify-atoms red pattern env))
      ((eq? red '_) env) ; wildcard: always unifies without side effect on env
      ((variable? red) ; variable: current value must match the pattern. If the variable is unbound, this becomes bound.
        (if (hash-has-key? env red)
              (let ((eredisting-value (hash-ref env red)))
                  (if (equal? eredisting-value pattern)
                      env
                      #f))
              (hash-set env red pattern)))
      ((and (pair? red) (eq? (car red) 'quote))
        (if (eq? (cadr red) pattern)
            env
            #f))
      ((equal? red pattern) env)
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
  (let loop ((work (set (cons (rule-body rule) (hash)))) ; The predicates to be checked. Initially, the predicates in the rule body.
             (ΔE   (set)))
    ;(printf "loop ~v\n" work)
    (if (set-empty? work)
        ΔE
        (match-let (((cons atoms env) (set-first work)))
        ;(printf "looking at atoms ~v in ~v\n" atoms env)
        (if (null? atoms)
            (let* ((hv (rule-head rule))
                   (new-fact (bind-fact hv env)))
              (loop (set-rest work) (set-add ΔE new-fact)))
            (let ((av (car atoms)))
              (match av
                ((¬ av) ; duplicating special terms, other strategy: let special forms return results one by one for "fail-fast"
                  (match av
                    ((vector '= p q)
                      (let* ((atoms-rest (cdr atoms))
                             (pp (evaluate-unquoted p env))
                             (qq (evaluate-unquoted q env))
                             (env* (unify-terms pp qq env)))
                        (if env* ; Test whether unification succeeded.
                            (loop (set-rest work) ΔE)
                            (loop (set-add (set-rest work) (cons (cdr atoms) env)) ΔE))))
                    (_
                      (let e-loop ((E E))
                        (if (set-empty? E)
                            (loop (set-add (set-rest work) (cons (cdr atoms) env)) ΔE)
                            (let* ((ev (set-first E))
                                   (env* (unify-atoms av ev env)))
                                ;(printf "¬ unify result ~a ~a: ~v\n" av ev m)
                              (if env*
                                  (loop (set-rest work) ΔE)
                                  (e-loop (set-rest E)))))))))
                ((vector 'DEBUG name)
                  (printf "~a: about to match ~a with ~a\n\n" name (cadr atoms) env)
                  (loop (set-add (set-rest work) (cons (cdr atoms) env)) ΔE))
                ((vector '%for-all x index l) ; for all (x, i) in l
                  (let ((d-lst (evaluate l env))
                        (atoms-rest (cdr atoms)))
                    (loop (for/fold ((work (set-rest work))) ((el d-lst) (i (in-naturals)))
                                        (set-add work (cons atoms-rest (hash-set (hash-set env index i) x el))))
                                      ΔE)))
                ((vector '%for-all x l) ; for all x in l
                  (let ((d-lst (evaluate l env)))
                    (let ((atoms-rest (cdr atoms)))
                      (if (null? d-lst)
                          (loop (set-rest work) ΔE)
                          (loop (for/fold ((work (set-rest work))) ((el d-lst))
                            (set-add work (cons atoms-rest (hash-set env x el))))
                            ΔE)))))
                ((vector '%select x index l) ; select x at index in l
                  (let ((d-index (evaluate index env)))
                    (let ((d-lst (evaluate l env)))
                      (let ((atoms-rest (cdr atoms)))
                          (if (or (null? d-lst) (>= d-index (length d-lst)))
                              (loop (set-rest work) ΔE)
                              (loop (set-add (set-rest work) (cons atoms-rest (hash-set env x (list-ref d-lst d-index)))) ΔE))))))
                ((vector '%index-of x index l) ; index of x in l
                  (let ((d-x (evaluate index env)))
                    (let ((d-lst (evaluate l env)))
                      (let ((atoms-rest (cdr atoms)))
                        (loop (set-add (set-rest work) (cons atoms-rest (hash-set env index (index-of d-lst d-x)))) ΔE))))) ; only finds first index
                ((vector '= p q)
                  (let ((atoms-rest (cdr atoms)))
                    (let ((pp (evaluate-unquoted p env)))
                      (let ((qq (evaluate-unquoted q env)))
                        (let ((env* (unify-terms pp qq env)))
                        (if env*
                            (loop (set-add (set-rest work) (cons atoms-rest env*)) ΔE)
                            (loop (set-rest work) ΔE)))))))
                ((vector '*Recent* p)
                  (let ((work (match-predicate p (cdr atoms) delta-tuples env (set-rest work))))
                    (loop work ΔE)))
                (_ 
                  (let ((work (match-predicate av (cdr atoms) E env (set-rest work))))
                    (loop work ΔE)))
                  )))))))

(define (match-predicate av remaining-preds E env work)
  (let e-loop ((E E) (work-acc work))
    (if (set-empty? E)
        work-acc
        (let ((ev (set-first E)))
          (let ((env* (unify-atoms av ev env)))
            ;(when m (printf "unify result: ~v\n" m))
            (if env*
                (e-loop (set-rest E) (set-add work-acc (cons remaining-preds env*)))
                (e-loop (set-rest E) work-acc)))))))

(define (solve-naive P E)

  (define (solve-naive-helper rules tuples)
    (let loop ((rules rules) (derived-tuples tuples))
      (if (set-empty? rules) ; Check whether all rules have been traversed as far as needed.
          derived-tuples
          (let ((rule (set-first rules)))
            (let ((derived-tuples-for-rule (fire rule derived-tuples (set)))) ; Fire the first rule.
              (loop (set-rest rules) (set-union derived-tuples derived-tuples-for-rule))))))) ; Accumulate all derived tuples.

  (define strata (stratify P))
  ;(printf "stratify ~v\n\n" strata)

  ; * E      set of facts (initially only the ones in the database)
  ; * strata list of strata
  (let inter-loop ((E-inter E) (S strata))
    (printf "\ninter ~a/~a with ~a facts\n" (- (set-count strata) (set-count S)) (set-count strata) (set-count E-inter))
    (if (null? S) ; Check whether there are more strata to traverse.
        E-inter ; All facts (initial and derived).
        (let ((Pi (car S))) ; Rules in the first stratum.
          (printf "Pi: ~v\n" (list->set (set-map Pi (lambda (r) (atom-name (rule-head r))))))
          (let intra-loop ((E-intra E-inter))
            (let ((tuples (solve-naive-helper Pi E-intra)))
              (let ((E-intra* (set-union E-intra tuples)))
                  (if (equal? E-intra E-intra*) ; monotonicity: size check quicker? (TODO)
                      (inter-loop E-intra* (cdr S))
                      (let ((new-facts (set-subtract E-intra* E-intra)))
                        (printf "new facts: ~a\n" new-facts)
                        (intra-loop E-intra*))))))))))


(define (solve-semi-naive P E)

  (define strata (stratify P))

  (let stratum-loop ((S strata) (E E))
    (printf "\ninter ~a/~a with ~a facts\n" (- (set-count strata) (set-count S)) (set-count strata) (set-count E))

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


(define (perform-iter rules tuples)

  (define semi-naive-rules (rewrite-semi-naive rules))
  (printf "semi-naive rules: ~a\ntuples: ~a\n" semi-naive-rules tuples)
  (define pred-name-to-rules
    (for/fold ((R (hash))) ((snr (in-set semi-naive-rules)))
      (for/fold ((R R)) ((term (in-list (rule-body snr))))
        (match term
          ((¬ p)
           (let ((term-name (atom-name p)))
            (hash-set R term-name (set-add (hash-ref R term-name (set)) snr)))) ; remember: no *Recent* possible!
          (_ 
            (let ((term-name (atom-name term)))
              (if (equal? term-name '*Recent*)
                  (let ((recent-term (vector-ref term 1)))
                    (let ((recent-name (atom-name recent-term)))
                      (hash-set R recent-name (set-add (hash-ref R recent-name (set)) snr))))
                  (hash-set R term-name (set-add (hash-ref R term-name (set)) snr)))))
          )))) ; always an EDB?
  ;(printf "pred-name-to-rules ~a\n" pred-name-to-rules)
  
  (let rule-loop ((rules* semi-naive-rules) (tuples tuples) (previous-delta-tuples tuples) (delta-tuples (set)))
    (let ((delta-rules
            (for/fold ((R (set))) ((previous-delta-tuple (in-set previous-delta-tuples)))
              (set-union R (hash-ref pred-name-to-rules (atom-name previous-delta-tuple) (set))))))
      ;(printf "delta rules :~a\n" delta-rules)
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