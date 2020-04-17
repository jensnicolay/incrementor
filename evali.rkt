#lang racket

(require "ast.rkt")
(require "datalog.rkt")
(require "naive.rkt")
(require "semi-naive.rkt")
(require "incremental.rkt")

(define (param->tuples p)
  (define i -1)
  (lambda (e-param)
    (set! i (add1 i))
    (match e-param
      ((«id» l x) (set `#(Param ,l ,x ,p ,i))))))

(define (arg->tuples p)
  (define i -1)
  (lambda (e-arg)
    (set! i (add1 i))
    (set-add
      (ast->tuples e-arg)
      `#(Arg ,(ast-label e-arg) ,p ,i))))

(define (ast->tuples e)
  (match e
    ((«id» l x) (set `#(Id ,l ,x)))
    ((«lit» l d) (set`#(Lit ,l ,d)))
    ((«lam» l (list e-params ...) e-body)
      (set-add
        (foldl set-union (ast->tuples e-body) (map (param->tuples l) e-params)) 
        `#(Lam ,l ,(ast-label e-body))))
    ; ((«lam» _ (list-rest e-params ... e-param) e-body) (set-add (set-add (list->set e-params) e-param) e-body))
    ; ((«lam» _ (? «id»? e-param) e-body) (set e-param e-body))
    ((«let» l x e0 e1)
      (set-add (set-union (ast->tuples x) (ast->tuples e0) (ast->tuples e1)) `#(Let ,l ,(ast-label x) ,(ast-label e0) ,(ast-label e1))))
    ((«letrec» l x e0 e1)
      (set-add (set-union (ast->tuples x) (ast->tuples e0) (ast->tuples e1)) `#(Letrec ,l ,(ast-label x) ,(ast-label e0) ,(ast-label e1))))
    ((«if» l ae e1 e2)
      (set-add (set-union (ast->tuples ae) (ast->tuples e1) (ast->tuples e2)) `#(If ,l ,(ast-label ae) ,(ast-label e1) ,(ast-label e2))))
    ; ((«car» _ x) (set x))
    ; ((«cdr» _ x) (set x))
    ; ((«set!» _ x ae) (set x ae))
    ; ((«set-car!» _ x ae) (set x ae))
    ; ((«set-cdr!» _ x ae) (set x ae))
    ; ((«cons» _ ae1 ae2) (set ae1 ae2))
    ; ((«make-vector» _ ae1 ae2) (set ae1 ae2))
    ; ((«vector-ref» _ x ae) (set x ae))
    ; ((«vector-set!» _ x ae1 ae2) (set x ae1 ae2))
    ; ((«quo» _ _) (set))
    ((«app» l e-rator e-rands)
      (set-add
        (foldl set-union (ast->tuples e-rator) (map (arg->tuples l) e-rands))
        `#(App ,l ,(ast-label e-rator))))
    (_ (error "ast->tuples: cannot handle expression" e))))

; Set of evaluation rules for Scheme, expressed as Datalog relations.
(define P (set

  ; Ast/1 succeeds when its argument is a node in the ast. e is the label (l) corresponding to this node.
  ; Let and Letrec are limited to binding a single variable.
  (#(Ast e) . :- . #(Lit e _))          
  (#(Ast e) . :- . #(Id e _))           
  (#(Ast e) . :- . #(Lam e _))        
  (#(Ast e) . :- . #(Let e _ _ _))      
  (#(Ast e) . :- . #(Letrec e _ _ _))   
  (#(Ast e) . :- . #(If e _ _ _))       
  (#(Ast e) . :- . #(App e _))        

  ; Parent/2 succeeds when both arguments represent (labels of) nodes in the AST and 
  ; the node represented by the second argument is the parent of the node represented by the first argument.
  (#(Parent e p) . :- . #(Let p e _ _)) ;;
  (#(Parent e p) . :- . #(Let p _ e _))
  (#(Parent e p) . :- . #(Let p _ _ e))
  (#(Parent e p) . :- . #(Letrec p e _ _)) ;;
  (#(Parent e p) . :- . #(Letrec p _ e _))
  (#(Parent e p) . :- . #(Letrec p _ _ e))
  ;(#(Parent e p) . :- . #(Lam p e-params _) #(%for-all e e-params))
  (#(Parent e p) . :- . #(Lam p e))
  (#(Parent e p) . :- . #(App p e))
  (#(Parent e p) . :- . #(Arg e p _))
  (#(Parent e p) . :- . #(If p e _ _))
  (#(Parent e p) . :- . #(If p _ e _))
  (#(Parent e p) . :- . #(If p _ _ e))

  ; Root/1 succeeds if its argument is the root node of the ast, i.e., a node without a parent.
  (#(HasParent e) . :- . #(Parent e _))
  (#(Root e) . :- . #(Ast e) (¬ #(HasParent e)))

  (:- `#(Prim "+" ,+))
  (:- `#(Prim "-" ,-))
  (:- `#(Prim "*" ,*))
  (:- `#(Prim "=" ,=))
  (:- `#(Prim "<" ,<))

  ; Reachable/2 succeeds when its first argument is the root node of the AST or when there exists is an expression
  ; that is reachable from the root node and steps to the expression in the first argument.
  (#(Reachable e 0) . :- . #(Root e))
  (#(Reachable e‘ κ‘) . :- . #(Reachable e κ) #(Step e κ e‘ κ‘))
  
  (#(Step e κ e‘ κ‘) . :- . #(Lit e _) #(Reachable e κ) #(Cont e κ e‘ κ‘))
  (#(Step e κ e‘ κ‘) . :- . #(Id e _) #(Reachable e κ) #(Cont e κ e‘ κ‘))
  (#(Step e κ e‘ κ‘) . :- . #(Lam e _) #(Reachable e κ) #(Cont e κ e‘ κ‘))
  (#(Step e κ e-init κ) . :- . #(Let e _ e-init _) #(Reachable e κ))
  (#(Step e κ e-init κ) . :- . #(Letrec e _ e-init _) #(Reachable e κ))
  (#(Step e κ e-body #(call e κ)) . :- . #(App e e-rator) #(Reachable e κ) #(Geval e-rator e κ #(obj e-lam _ _)) #(Lam e-lam e-body))
  (#(Step e κ e‘ κ‘) . :- . #(App e e-rator) #(Reachable e κ) #(Geval e-rator e κ #(prim _)) #(Cont e κ e‘ κ‘))
  (#(Step e κ e-then κ) . :- . #(If e e-cond e-then _) #(Reachable e κ) #(Geval e-cond e κ d) (¬ #(= d #f)))
  (#(Step e κ e-else κ) . :- . #(If e e-cond _ e-else) #(Reachable e κ) #(Geval e-cond e κ #f))
  
  (#(Cont e-init κ e-body κ) . :- . #(Let p _ e-init e-body) #(Parent e p) #(Reachable e κ))
  (#(Cont e-init κ e-body κ) . :- . #(Letrec p _ e-init e-body) #(Parent e p) #(Reachable e κ))
  (#(Cont e-body κ e‘ κ‘) . :- . #(Let p _ _ e-body) #(Parent e-body p) #(Reachable e-body κ) #(Cont p κ e‘ κ‘))
  (#(Cont e-body κ e‘ κ‘) . :- . #(Letrec p _ _ e-body) #(Parent e-body p) #(Reachable e-body κ) #(Cont p κ e‘ κ‘))
  (#(Cont e-then κ e‘ κ‘) . :- . #(If p _ e-then _) #(Parent e-then p) #(Reachable e-then κ) #(Cont p κ e‘ κ‘))
  (#(Cont e-else κ e‘ κ‘) . :- . #(If p _ _ e-else) #(Parent e-then p) #(Reachable e-then κ) #(Cont p κ e‘ κ‘))
  (#(Cont e-body κ e‘ κ‘) . :- . #(Lam p e-body) #(Parent e-body p) #(Step e-call κ-call e-body κ) #(Cont e-call κ-call e‘ κ‘))

  (#(Binds e x) . :- . #(Param _ x e _))

  (#(Evaluated e e κ) . :- . #(Lit e _) #(Reachable e κ))
  (#(Evaluated e e κ) . :- . #(Id e _) #(Reachable e κ))
  (#(Evaluated e e κ) . :- . #(Lam e _) #(Reachable e κ))
  (#(Evaluated e-rator e κ) . :- .  #(App e e-rator) #(Reachable e κ))
  (#(Evaluated e-rand e κ) . :- . #(App e _) #(Arg e-rand e _) #(Reachable e κ))
  (#(Evaluated e-cond e κ) . :- . #(If e e-cond _ _) #(Reachable e κ))

  (#(Lookup-root x e-body κ #(root e-init e-init κ)) . :- . #(Let e e-id e-init e-body) #(Id e-id x) #(Reachable e κ))
  (#(Lookup-root x e-init κ #(root e-init e-init κ)) . :- . #(Letrec e e-id e-init _) #(Id e-id x) #(Reachable e κ))
  (#(Lookup-root x e-body κ #(root e-init e-init κ)) . :- . #(Letrec e e-id e-init e-body) #(Id e-id x) #(Reachable e κ))
  (#(Lookup-root x e-init κ r) . :- . #(Lookup-root x p κ r) #(Let p _ e-init _))
  (#(Lookup-root x e-body κ r) . :- . #(Lookup-root x p κ r) #(Let p e-id _ e-body) (¬ #(Id e-id x)))
  (#(Lookup-root x e-init κ r) . :- . #(Lookup-root x p κ r) #(Letrec p e-id e-init _) (¬ #(Id e-id x)))
  (#(Lookup-root x e-body κ r) . :- . #(Lookup-root x p κ r) #(Letrec p e-id _ e-body) (¬ #(Id e-id x)))
  (#(Lookup-root x e-cond κ r) . :- . #(Lookup-root x p κ r) #(If p e-cond _ _))
  (#(Lookup-root x e-then κ r) . :- . #(Lookup-root x p κ r) #(If p _ e-then _))
  (#(Lookup-root x e-else κ r) . :- . #(Lookup-root x p κ r) #(If p _ _ e-else))
  (#(Lookup-root x e-body κ‘ #(root e-rand e κ)) . :- . #(App e e-rator) #(Lam e-lam e-body)
                                                         #(Param e-param x e-lam i) #(Arg e-rand e i) #(Step e κ e-body κ‘))
  (#(Lookup-root x e-body κ‘ r) . :- . #(App e e-rator) #(Geval e-rator e κ #(obj e-lam e-obj κ-obj))
                                        #(Lookup-root x e-obj κ-obj r) (¬ #(Binds e-lam x)) #(Step e κ e-body κ‘))
  (#(Lookup-root "+" e κ #f) . :- . #(Root e) #(Reachable e κ))
  (#(Lookup-root "-" e κ #f) . :- . #(Root e) #(Reachable e κ))
  (#(Lookup-root "*" e κ #f) . :- . #(Root e) #(Reachable e κ))
  (#(Lookup-root "=" e κ #f) . :- . #(Root e) #(Reachable e κ))
  (#(Lookup-root "<" e κ #f) . :- . #(Root e) #(Reachable e κ))
  
  (#(Geval e‘ e κ d) . :- . #(Lit e‘ d) #(Evaluated e‘ e κ))
  (#(Geval e‘ e κ d) . :- . #(Id e‘ x) #(Evaluated e‘ e κ) #(Lookup-root x e κ #(root e-r e-rs κ-rs)) #(Geval e-r e-rs κ-rs d))
  (#(Geval e‘ e κ #(prim proc)) . :- . #(Id e‘ x) #(Evaluated e‘ e κ) #(Lookup-root x e κ #f) #(Prim x proc))
  (#(Geval e‘ e κ #(obj e‘ e κ)) . :- . #(Lam e‘ _) #(Evaluated e‘ e κ))
  (#(Geval e e κ d) . :- . #(Let e _ _ e-body) #(Reachable e κ) #(Geval e-body e-body κ d))
  (#(Geval e e κ d) . :- . #(Letrec e _ _ e-body) #(Reachable e κ) #(Geval e-body e-body κ d))
  (#(Geval e e κ d) . :- . #(App e _) #(Step e κ e-body κ‘) #(Lam _ e-body) #(Geval e-body e-body κ‘ d))
  (#(Geval e e κ d) . :- . #(App e e-rator) #(Reachable e κ) 
                            #(Geval e-rator e κ #(prim proc)) #(Arg e1 e 0) #(Geval e1 e κ d1) #(Arg e2 e 1) #(Geval e2 e κ d2) #(= d ,(proc d1 d2)))
  (#(Geval e e κ d) . :- . #(If e _ _ _) #(Step e κ e-thenelse κ) #(Geval e-thenelse e-thenelse κ d))
  
  ; Final/2 succeeds when its first argument is an expression that cannot be evaluated further, i.e., which cannot be stepped anymore.
  (#(Steps e κ) . :- . #(Step e κ _ _))
  (#(Final e κ) . :- . #(Reachable e κ) (¬ #(Steps e κ)))

  ; Eval/2 succeeds when its first argument can be evaluated to a result, which is/should be its second argument.
  (#(Eval e d) . :- . #(Final e κ) #(Geval e e κ d)) 
))

(define (singleton-result result)
  (if (not (= 1 (set-count result)))
      (error "wrong number of results: " result)
      (set-first result)))

(define (label-to-ast-tuple l delta-solver)
  
  (define tuples (delta-solver 'tuples))
  
  (or
    (for/or ((tuple (in-set tuples)))
      (match tuple
        ((vector 'Lit (== l) _) tuple)
        ((vector 'Id (== l) _) tuple)
        ((vector 'Let (== l) _ _ _) tuple)
        ((vector 'If (== l) _ _ _) tuple)
        ((vector 'App (== l) _) tuple)
        (_ #f)))
    (error "cannot find ast tuple with label" l)))

(define (tuple-to-ast delta-solver)
  
  (define tuples (delta-solver 'tuples))

  (define (arg-builder l)
    (let ((matches (delta-solver 'match-atom `#(Arg e ,l i))))
        (map builder (map (lambda (a) (vector-ref a 1)) (sort (set-map matches car) < #:key (lambda (a) (vector-ref a 3)))))))

  (define (builder* tuple)
    (match tuple
      (`#(Lit ,l ,d) («lit» l d))
      (`#(Id ,l ,x) («id» l x))
      (`#(Let ,l ,e₀ ,e₁ ,e₂) («let» l (builder e₀) (builder e₁) (builder e₂)))
      (`#(If ,l ,e₀ ,e₁ ,e₂) («if» l (builder e₀) (builder e₁) (builder e₂)))
      (`#(App ,l ,e₀) («app» l (builder e₀) (arg-builder l)))
      (_ (error "cannot handle ast tuple" tuple))))

  (define (builder l)
    (builder* (label-to-ast-tuple l delta-solver)))

  builder*)

(define (parentl e delta-solver)
  (let ((m (singleton-result (delta-solver 'match-atom `#(Parent ,(ast-label e) p)))))
    (let ((p (hash-ref (cdr m) 'p)))
      p)))
      ;((label-to-ast delta-solver) p))))

(define (evali e)
  
  (define (cont-with-result sr)
    (match-let (((solver-result tuples num-der-tuples* delta-solver) sr))
      (printf "(~a tuples, ~a derived, ~a removed)\n" (set-count tuples) num-der-tuples* (delta-solver 'num-removed-tuples))
      ;(printf "RESULT: ~a\n" tuples)
      (let ((Root (sequence->list (sequence-filter (lambda (a) (eq? 'Root (atom-name a))) (in-set tuples)))))
        (unless (= 1 (length Root))
          (error 'conc-eval "wrong number of roots: ~a" Root))
        (let ((Eval (sequence->list (sequence-filter (lambda (a) (eq? 'Eval (atom-name a))) (in-set tuples)))))
          (if (= (length Eval) 1)
              (let ((result (vector-ref (car Eval) 2)))
                (lambda msg
                  (match msg
                    (`(result) result)
                    (`(provenance) (delta-solver 'provenance))
                    (`(parent ,e)
                      (tuple-to-ast (parentl e delta-solver) delta-solver))
                    (`(program)
                      ((tuple-to-ast delta-solver) (label-to-ast-tuple (vector-ref (car Root) 1) delta-solver)))
                    (`(replace ,e₀ ,e₁)
                        (let ((p (label-to-ast-tuple (parentl e₀ delta-solver) delta-solver)))
                          (match p
                            ((vector 'Let l x e₀ e-body)
                              (let ((delta
                                (append
                                  (list
                                    (remove-tuple (label-to-ast-tuple e₀ delta-solver))
                                    (remove-tuple p)
                                    (add-tuple `#(Let ,l ,x ,(ast-label e₁) ,e-body)))
                                  (set-map (ast->tuples e₁) add-tuple))))
                                (cont-with-result (delta-solver 'apply-delta delta)))))))
                    (_ (error "evali: cannot understand" msg)))))
              (error 'conc-eval "wrong Eval result: ~a\n~a" Eval (sort-tuples tuples)))))))

  (let ((E (ast->tuples e)))
    (printf "tuples (~a): ~a\n" (set-count E) E)
    (cont-with-result (solve-incremental P E))))

(define compile (make-compiler))
(define let-binding (compile 2))
(define p1 (compile
  `(let ((x ,let-binding))
    (let ((c (< x 0)))
      (if c
          'neg
          'zeropos)))))


(define ii (evali p1))
(ast->string (ii 'program))
(ii 'result)

(define let-binding‘ (compile -3))
(define ii1 (ii 'replace let-binding let-binding‘))
(ast->string (ii1 'program))
(ii1 'result)

(define let-binding“ (compile 2))
(define ii2 (ii1 'replace let-binding‘ let-binding“))
(ast->string (ii2 'program))
(ii2 'result)


