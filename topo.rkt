#lang racket
 
(define G 
  (hash 
   'des_system_lib (set 'std 'synopsys 'std_cell_lib 'des_system_lib 'dw02 'dw01 'ramlib 'ieee)
   'dw01           (set 'ieee 'dw01 'dware 'gtech)
   'dw02           (set 'ieee 'dw02 'dware)
   'dw03           (set 'std 'synopsys 'dware 'dw03 'dw02 'dw01 'ieee 'gtech)
   'dw04           (set 'dw04 'ieee 'dw01 'dware 'gtech)
   'dw05           (set 'dw05 'ieee 'dware)
   'dw06           (set 'dw06 'ieee 'dware)
   'dw07           (set 'ieee 'dware)
   'dware          (set 'ieee 'dware)
   'gtech          (set 'ieee 'gtech)
   'ramlib         (set 'std 'ieee)
   'std_cell_lib   (set 'ieee 'std_cell_lib)
   'synopsys       (set)))
 
(define (clean G)
  (define G* (hash-copy G))
  (for ([(from tos) G])
    ; remove self dependencies
    (hash-set! G* from (set-remove tos from))
    ; make sure all nodes are present in the ht
    (for ([to tos]) (hash-update! G* to (λ(_)_) (set))))
  G*)
 
(define (incoming G)
  (define in (make-hash))
  (for* ([(from tos) G] [to tos])
    (hash-update! in to (λ(fs) (set-add fs from)) (set)))
  in)
 
(define (nodes G)       (hash-keys G))
(define (out G n)       (hash-ref G n (set)))
(define (remove! G n m) (hash-set! G n (set-remove (out G n) m)))
 
(define (topo-sort G)
  (define n (length (nodes G)))
  (define in (incoming G))
  (define (no-incoming? n) (set-empty? (hash-ref in n (set))))
  (let loop ([L '()] [S (list->set (filter no-incoming? (nodes G)))])
    (cond [(set-empty? S)
           (if (= (length L) n)
               L
               (error 'topo-sort (~a "cycle detected" G)))]
          [else 
           (define n   (set-first S))
           (define S\n (set-rest S))                
           (for ([m (out G n)])
             (remove! G n m)
             (remove! in m n)
             (when (no-incoming? m)
               (set! S\n (set-add S\n m))))
           (loop (cons n L) S\n)])))
 
(topo-sort (clean G))