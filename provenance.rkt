#lang racket

; TODO; investigate BDDs

(provide add add-product remove-variables-from-system remove-variables)
   
(define (add n1 n2)
  (for/fold ((result n1)) ((p (in-set n2)))
    (add-product result p)))

(define (add-product n p)
  (let ((n* (set-add n p)))
    (absorption n*)))

(define (multiply n1 n2)
  (for/fold ((result (set))) ((p1 (in-set n1)))
    (for/fold ((result result)) ((p2 (in-set n2)))
      (set-add result (set-union p1 p2)))))

(define (absorption n)

  (define (perform p n)
    (if (null? n)
        '()
        (let ((p* (car n)))
          (if (subset? p p*)
              (perform p (cdr n))
              (cons p* (perform p (cdr n)))))))
    
  (define (absorption* n)
    (if (null? n)
        (set)
        (let ((p (car n)))
          (set-add (absorption* (perform p (cdr n))) p))))

  (let ((n* (sort (set->list n) < #:key set-count)))
    (absorption* n*)))
      

; (define (remove-variable n x)
;   (for/fold ((result (set))) ((p (in-set n)))
;     (if (set-member? p x)
;         result
;         (set-add result p))))

(define (remove-variables n xs)
  (for/fold ((result (set))) ((p (in-set n)))
    (if (for/or ((x (in-set p)))
          (set-member? xs x))
        result
        (set-add result p))))


; (define (simplify pred busy T P)
;   (let ((n (hash-ref P pred #f)))
;     (if n
;         (values n P)
;         (let-values (((simplified P**)
;             (let ((n (hash-ref T pred #f)))
;               (if n
;                   (for/fold ((result (set)) (P P)) ((p (in-set n)))
;                     (if (set-empty? (set-intersect busy p))
;                         (let-values (((n* P*) (simplify-product p busy T P)))
;                           (values (add result n*) P*))
;                         (values result P)))
;                   (values (set (set pred)) P))))) ; this acts as an EDB -> EDB ; TODO simplify to `pred` iso. `(set (set pred))`?
;           (values simplified (hash-set P** pred simplified))))))

; ORIGINAL BEFORE ¬
; (define (remove-variables-from-system system xs)
;   (let var-loop ((system (hash->list system)) (xs xs))
;     (if (set-empty? xs)
;         (make-immutable-hash system)
;         (let ((fully-removed-var (set-first xs))) ; only this var is guaranteed to be fully removed after system loop
;           (let system-loop ((system system) (system* '()) (xs xs))
;             (if (null? system)
;                 (var-loop system* (set-remove xs fully-removed-var)) ; TODO test by keeping additional "work remove list"
;                 (let ((prov-kv (car system)))
;                   (let ((prov* (remove-variables (cdr prov-kv) (set-add xs (car prov-kv))))) ; also remove circular stuff
;                     (if (set-empty? prov*)
;                         (system-loop (cdr system) system* (set-add xs (car prov-kv))) ; remove from system, add var to remove-set
;                         (system-loop (cdr system) (cons (cons (car prov-kv) prov*) system*) xs))))))))))

(define (remove-variables-from-system system xs)
  (let var-loop ((system (hash->list system)) (xs xs))
    (if (set-empty? xs)
        (make-immutable-hash system)
        (let ((fully-removed-var (set-first xs))) ; only this var is guaranteed to be fully removed after system loop
          (let system-loop ((system system) (system* '()) (xs xs))
            (if (null? system)
                (var-loop system* (set-remove xs fully-removed-var)) ; TODO test by keeping additional "work remove list"
                (let ((prov-kv (car system)))
                  (let ((prov* (remove-variables (cdr prov-kv) (set-add xs (car prov-kv))))) ; also remove circular stuff
                    (if (set-empty? prov*)
                        (system-loop (cdr system) system* (set-add xs (car prov-kv))) ; remove from system, add var to remove-set
                        (system-loop (cdr system) (cons (cons (car prov-kv) prov*) system*) xs))))))))))


; (define (simplify-product p busy T P)
;   (for/fold ((result (set (set))) (P P)) ((pp (in-set p)))
;     (let-values (((n P*) (simplify pp (set-add busy pp) T P)))
;       (values (multiply result n) P*))))



(module+ main
  (define sum set)
  (define product set)
  (define T
      (hash 'R5 (sum (product 'L1) (product 'L1 'R12))
            'R6 (sum (product 'L2) (product 'L2 'R10))
            'R7 (sum (product 'L3) (product 'L4 'R11) (product 'L3 'R13))
            'R8 (sum (product 'L4) (product 'L3 'R5) (product 'L4 'R12))
            'R9 (sum (product 'L1 'R6))
            'R10 (sum (product 'L4 'R6) (product 'L3 'R9))
            'R11 (sum (product 'L2 'R7))
            'R12 (sum (product 'L2 'R8))
            'R13 (sum (product 'L1 'R11))))

  (define T* (remove-variables-from-system T (set 'L4)))
  (remove-variables-from-system T* (set 'L3))
)
