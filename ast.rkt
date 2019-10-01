#lang racket
(provide (all-defined-out))

; Types of AST nodes.
(struct «lit» (l v) #:transparent)
(struct «id» (l x) #:transparent)
(struct «quo» (l e) #:transparent)
(struct «lam» (l x e) #:transparent)
(struct «app» (l ae aes) #:transparent)
(struct «let» (l x e0 e1) #:transparent)
(struct «letrec» (l x e0 e1) #:transparent)
(struct «if» (l ae e0 e1) #:transparent)
(struct «set!» (l x ae) #:transparent)
(struct «car» (l x) #:transparent)
(struct «cdr» (l x) #:transparent) 
(struct «set-car!» (l x ae) #:transparent)
(struct «set-cdr!» (l x ae) #:transparent)
(struct «cons» (l ae1 ae2) #:transparent)
(struct «vector-ref» (l x ae) #:transparent)
(struct «vector-set!» (l x ae1 ae2) #:transparent)
(struct «make-vector» (l ae1 ae2) #:transparent)

; Retrieves the label l of an AST node e.
; This tag is unique for each node (see make-compiler).
(define (ast-label e)
  (match e
    ((«id» l _) l)
    ((«lit» l _) l)
    ((«lam» l _ _) l)
    ((«let» l _ _ _) l)
    ((«letrec» l _ _ _) l)
    ((«if» l _ _ _) l)
    ((«car» l _) l)
    ((«cdr» l _) l)
    ((«set!» l _ _) l)
    ((«set-car!» l _ _) l)
    ((«set-cdr!» l _ _) l)
    ((«cons» l _ _) l)
    ((«make-vector» l _ _) l)
    ((«vector-ref» l _ _) l)
    ((«vector-set!» l _ _ _) l)
    ((«quo» l _) l)
    ((«app» l _ _) l)
    (_ (error "ast-label: cannot handle expression" e))))


(define (make-compiler)
  ; Last used tag for AST nodes.
  (define l -1)         
  ; Move to the next tag and return this.
  (define (tag!)        
    (set! l (add1 l))
    l)
  ; Compiles an expression into AST nodes.  
  (define (compile e)
    (match e

      ; Values
      ((? symbol?  v) («id» (tag!) (symbol->string v)))
      ((? boolean? v) («lit» (tag!) v))
      ((? number?  v) («lit» (tag!) v))
      ((? string?  v) («lit» (tag!) v))
      ((? char?    v) («lit» (tag!) v))
      ((? null?    v) '())

      ; Complex expressions & special forms
      (`(quote ,e) (compile-quote e))
      (`(lambda ,x ,e)              («lam»          (tag!) (compile-params x) (compile e)))
      (`(if ,ae ,e1 ,e2)            («if»           (tag!) (compile ae) (compile e1) (compile e2)))
      (`(let ((,x ,e0)) ,e1)        («let»          (tag!) (compile x) (compile e0) (compile e1)))
      (`(letrec ((,x ,e0)) ,e1)     («letrec»       (tag!) (compile x) (compile e0) (compile e1)))
      (`(set! ,x ,ae)               («set!»         (tag!) (compile x) (compile ae)))
      (`(car ,x)                    («car»          (tag!) (compile x)))
      (`(cdr ,x)                    («cdr»          (tag!) (compile x)))
      (`(cons ,ae1 ,ae2)            («cons»         (tag!) (compile ae1) (compile ae2)))
      (`(set-car! ,x ,ae)           («set-car!»     (tag!) (compile x) (compile ae)))
      (`(set-cdr! ,x ,ae)           («set-cdr!»     (tag!) (compile x) (compile ae)))        
      (`(vector-ref ,x ,ae)         («vector-ref»   (tag!) (compile x) (compile ae)))
      (`(vector-set! ,x ,ae1 ,ae2)  («vector-set!»  (tag!) (compile x) (compile ae1) (compile ae2)))
      (`(make-vector ,ae1 ,ae2)     («make-vector»  (tag!) (compile ae1) (compile ae2)))
      (`(,rator . ,rands)           («app»          (tag!) (compile rator) (map compile rands)))

      ; Other?
      ((? «id»?)     e)
      ((? «lam»?)    e)
      ((? «let»?)    e)
      ((? «letrec»?) e)
      ((? «if»?)     e)
      ((? «set!»?)   e)
      ((? «quo»?)    e)
      ((? «app»?)    e)
      ((? «lit»?)    e)
      (_ (error "compile: cannot handle expression" e))))
  ; Compiles a quoted expression e. If the expression represents a list, this list is built.
  ; If the expression is a symbol, a literal is returned. Otherwise, the expression is just compiled.
  (define (compile-quote e)
    (match e
      ((cons e-car e-cdr) («cons» (tag!) (compile-quote e-car) (compile-quote e-cdr)))
      ('() («lit» (tag!) e))
      ((? symbol? x) («lit» (tag!) x))
      (_ (compile e))))
  ; Compiles the parameters of a lambda expression.
  (define (compile-params es)
    (match es
      ((cons e-car e-cdr) (cons (compile e-car) (compile-params e-cdr)))
      (_ (compile es))))
  compile)

; Atomic expressions are literals, identifiers (variables), lambdas and quoted expressions.         
(define (ae? e)
  (match e
    ((«lit» _ _)   #t)
    ((«id» _ _)    #t)
    ((«lam» _ _ _) #t)
    ((«quo» _ e)   (not (pair? e))) 
    (_             #f)))

; Retrieves the children of an AST node.
(define (children e)
  (match e
    ((«id» _ _) (set))
    ((«lit» _ _) (set))
    ((«lam» _ (list e-params ...) e-body) (set-add (list->set e-params) e-body))
    ((«lam» _ (list-rest e-params ... e-param) e-body) (set-add (set-add (list->set e-params) e-param) e-body))
    ((«lam» _ (? «id»? e-param) e-body) (set e-param e-body))
    ((«let» _ x e0 e1) (set x e0 e1))
    ((«letrec» _ x e0 e1) (set x e0 e1))
    ((«if» _ ae e1 e2) (set ae e1 e2))
    ((«car» _ x) (set x))
    ((«cdr» _ x) (set x))
    ((«set!» _ x ae) (set x ae))
    ((«set-car!» _ x ae) (set x ae))
    ((«set-cdr!» _ x ae) (set x ae))
    ((«cons» _ ae1 ae2) (set ae1 ae2))
    ((«make-vector» _ ae1 ae2) (set ae1 ae2))
    ((«vector-ref» _ x ae) (set x ae))
    ((«vector-set!» _ x ae1 ae2) (set x ae1 ae2))
    ((«quo» _ _) (set))
    ((«app» _ rator rands) (set-add (list->set rands) rator))
    (_ (error "children: cannot handle expression" e))))

;(define (parent e ast)
;  (let ((cs (children ast)))
;    (if (set-member? cs e)
;        ast
;        (let loop ((cs cs))
;          (if (set-empty? cs)
;              #f
;              (let ((p (parent e (set-first cs))))
;                (or p (loop (set-rest cs)))))))))

(define (ast->string e)
  (match e
    ((«id» _ x) x)
    ((«lit» _ d) (~s d))
    ((«lam» _ x e) (format "(lambda ~a ~a)" (ast->string x) (ast->string e)))
    ((«let» _ x e0 e1) (format "(let ((~a ~a)) ~a)" (ast->string x) (ast->string e0) (ast->string e1)))
    ((«letrec» _ x e0 e1) (format "(letrec ((~a ~a)) ~a)" (ast->string x) (ast->string e0) (ast->string e1)))
    ((«if» _ ae e0 e1) (format "(if ~a ~a ~a)" (ast->string ae) (ast->string e0) (ast->string e1)))
    ((«car» _ x) (format "(car ~a)" (ast->string x)))
    ((«cdr» _ x) (format "(cdr ~a)" (ast->string x)))
    ((«set!» _ x ae) (format "(set! ~a ~a)" (ast->string x) (ast->string ae)))
    ((«set-car!» _ x ae) (format "(set-car! ~a ~a)" (ast->string x) (ast->string ae)))
    ((«set-cdr!» _ x ae) (format "(set-cdr! ~a ~a)" (ast->string x) (ast->string ae)))
    ((«cons» _ ae1 ae2) (format "(cons ~a ~a)" (ast->string ae1) (ast->string ae2)))
    ((«make-vector» _ ae1 ae2) (format "(make-vector ~a ~a)" (ast->string ae1) (ast->string ae2)))
    ((«vector-ref» _ x ae) (format "(vector-ref ~a ~a)" (ast->string x) (ast->string ae)))
    ((«vector-set!» _ x ae1 ae2) (format "(vector-set! ~a ~a ~a)" (ast->string x) (ast->string ae1) (ast->string ae2)))
    ((«quo» _ e) (format "'~a" (ast->string e)))
    ((«app» _ rator rands) (format "~a" (map ast->string (cons rator rands))))
    ((cons e0 e1) (cons (ast->string e0) (ast->string e1)))
    ('() "")
    (_ (error "ast->string: cannot handle expression" e))))
                                 

(define (parent-map ast)
  (define (traverse-ast S W)
    (if (set-empty? W)
        S
        (let* ((e (set-first W))
               (E* (children e))
               (S* (for/fold ((S S)) ((e* E*))
                     (hash-set S e* e)))
               (W* (set-union (set-rest W) E*)))
          (traverse-ast S* W*))))
  (traverse-ast (hash) (set ast)))

(define (make-parent ast)
  (let ((P (parent-map ast)))
    (lambda (e)
      (hash-ref P e #f))))

(define (nodes ast) (for/fold ((cs (list ast))) ((c (children ast))) (append cs (nodes c))))

(define (free e)
  (define (f e env)
    (match e
      ((«id» _ x) (if (set-member? env x)
                      (set)
                      (set x)))
      ((«lam» _ x e) (f e (set-union env (list->set (map «id»-x x)))))
      ((«let» _ x e0 e1) (set-union (f e0 env) (f e1 (set-add env («id»-x x)))))
      ((«letrec» _ x e0 e1) (set-union (f e0 (set-add env («id»-x x))) (f e1 (set-add env («id»-x x)))))
      ((«if» _ ae e1 e2) (set-union (f ae env) (f e1 env) (f e2 env)))
      ((«set!» _ x ae) (set-union (f x env) (f ae env)))
      ((«car» _ x) (f x env))
      ((«cdr» _ x) (f x env))
      ((«set-car!» _ x ae) (set-union (f x env) (f ae env)))
      ((«set-cdr!» _ x ae) (set-union (f x env) (f ae env)))
      ((«cons» _ ae1 ae2) (set-union (f ae1 env) (f ae2 env)))
      ((«make-vector» _ ae1 ae2) (set-union (f ae1 env) (f ae2 env)))
      ((«vector-ref» _ x ae) (set-union (f x env) (f ae env)))
      ((«vector-set!» _ x ae1 ae2) (set-union (f x env) (f ae1 env) (f ae2 env)))
      ((«quo» _ _) (set))
      ((«app» _ rator rands) (set-union (f rator env) (for/fold ((xs (set))) ((rand rands)) (set-union xs (f rand env)))))
      ((«id» _ _) (set))
      ((«lit» _ _) (set))
      (_ (error "cannot handle expression" e))))
  (f e (set)))

(module+ main

  (define compile (make-compiler))

  (let ((ast
    (compile '(let ((f (lambda () (- 5 3)))) (f)))
        ))

    (for-each (lambda (n) (printf "~v\n" (ast->string n))) (nodes ast)))


  ; (compile
  ;   ''(a b c)
  ; )
  
)


