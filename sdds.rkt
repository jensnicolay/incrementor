#lang racket

(require "ast.rkt")
(require "incrementor.rkt")

(define (ast->facts e)
  (let loop ((W (set e)) (E (set)))
    (if (set-empty? W)
        E
        (match (set-first W)
          ((«id» l x) (loop (set-rest W) (set-add E `#(Id ,l ,x))))
          ((«lit» l d) (loop (set-rest W) (set-add E `#(Lit ,l ,d))))
          ((«lam» l (list e-params ...) e-body)
            (loop (set-union (set-rest W) (list->set e-params) (set e-body))
                  (set-add E `#(Lam ,l ,(map ast-label e-params) ,(ast-label e-body)))))
          ; ((«lam» _ (list-rest e-params ... e-param) e-body) (set-add (set-add (list->set e-params) e-param) e-body))
          ; ((«lam» _ (? «id»? e-param) e-body) (set e-param e-body))
          ((«let» l x e0 e1)
            (loop (set-union (set-rest W) (set x e0 e1)) (set-add E `#(Let ,l ,(ast-label x) ,(ast-label e0) ,(ast-label e1)))))
          ((«letrec» l x e0 e1)
            (loop (set-union (set-rest W) (set x e0 e1)) (set-add E `#(Letrec ,l ,(ast-label x) ,(ast-label e0) ,(ast-label e1)))))
          ((«if» l ae e1 e2)
            (loop (set-union (set-rest W) (set ae e1 e2)) (set-add E `#(If ,l ,(ast-label ae) ,(ast-label e1) ,(ast-label e2)))))
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
            (loop (set-union (set-rest W) (set e-rator) (list->set e-rands)) (set-add E `#(App ,l ,(ast-label e-rator) ,(map ast-label e-rands)))))
          (_ (error "ast->facts: cannot handle expression" e))))))

(define P (set

  (#(Ast e) . :- . #(Lit e _))
  (#(Ast e) . :- . #(Id e _))
  (#(Ast e) . :- . #(Lam e _ _))
  (#(Ast e) . :- . #(Let e _ _ _))
  (#(Ast e) . :- . #(Letrec e _ _ _))
  (#(Ast e) . :- . #(If e _ _ _))
  (#(Ast e) . :- . #(App e _ _))

  (#(Parent e p) . :- . #(Let p e _ _)) ;;
  (#(Parent e p) . :- . #(Let p _ e _))
  (#(Parent e p) . :- . #(Let p _ _ e))
  (#(Parent e p) . :- . #(Letrec p e _ _)) ;;
  (#(Parent e p) . :- . #(Letrec p _ e _))
  (#(Parent e p) . :- . #(Letrec p _ _ e))
  (#(Parent e p) . :- . #(Lam p e-params _) #(∈1 e e-params))
  (#(Parent e p) . :- . #(Lam p _ e))
  (#(Parent e p) . :- . #(App p e _))
  (#(Parent e p) . :- . #(App p _ e-rands) #(∈1 e e-rands))
  (#(Parent e p) . :- . #(If p e _ _))
  (#(Parent e p) . :- . #(If p _ e _))
  (#(Parent e p) . :- . #(If p _ _ e))

  (#(Root e) . :- . #(Ast e) (¬ #(Parent e _)))

  (#(Reachable e 0) . :- . #(Root e))
  (#(Reachable e‘ κ‘) . :- . #(Reachable e κ) #(Step e κ e‘ κ‘))
  
  (#(Step e κ e‘ κ‘) . :- . #(Lit e _) #(Reachable e κ) #(Cont e κ e‘ κ‘))
  (#(Step e κ e‘ κ‘) . :- . #(Id e _) #(Reachable e κ) #(Cont e κ e‘ κ‘))
  (#(Step e κ e‘ κ‘) . :- . #(Lam e _ _) #(Reachable e κ) #(Cont e κ e‘ κ‘))
  (#(Step e κ e-init κ) . :- . #(Let e _ e-init _) #(Reachable e κ))
  (#(Step e κ e-init κ) . :- . #(Letrec e _ e-init _) #(Reachable e κ))
  (#(Step e κ e-body #(call e κ)) . :- . #(App e e-rator _) #(Reachable e κ) #(Geval e-rator e κ #(obj e-lam _ _)) #(Lam e-lam _ e-body))
  (#(Step e κ e‘ κ‘) . :- . #(App e e-rator _) #(Reachable e κ) #(Geval e-rator e κ #(prim _)) #(Cont e κ e‘ κ‘))
  (#(Step e κ e-then κ) . :- . #(If e e-cond e-then _) #(Reachable e κ) #(Geval e-cond e κ d) (¬ #(= d #f)))
  (#(Step e κ e-else κ) . :- . #(If e e-cond _ e-else) #(Reachable e κ) #(Geval e-cond e κ #f))
  
  (#(Cont e-init κ e-body κ) . :- . #(Let p _ e-init e-body) #(Parent e p) #(Reachable e κ))
  (#(Cont e-init κ e-body κ) . :- . #(Letrec p _ e-init e-body) #(Parent e p) #(Reachable e κ))
  (#(Cont e-body κ e‘ κ‘) . :- . #(Let p _ _ e-body) #(Parent e-body p) #(Reachable e-body κ) #(Cont p κ e‘ κ‘))
  (#(Cont e-body κ e‘ κ‘) . :- . #(Letrec p _ _ e-body) #(Parent e-body p) #(Reachable e-body κ) #(Cont p κ e‘ κ‘))
  (#(Cont e-then κ e‘ κ‘) . :- . #(If p _ e-then _) #(Parent e-then p) #(Reachable e-then κ) #(Cont p κ e‘ κ‘))
  (#(Cont e-else κ e‘ κ‘) . :- . #(If p _ _ e-else) #(Parent e-then p) #(Reachable e-then κ) #(Cont p κ e‘ κ‘))
  (#(Cont e-body κ e‘ κ‘) . :- . #(Lam p _ e-body) #(Parent e-body p) #(Step e-call κ-call e-body κ) #(Cont e-call κ-call e‘ κ‘))

  (#(Binds e x) . :- . #(Lam e e-params _) #(∈1 e-param e-params) #(Id e-param x))

  (#(Evaluated e e κ) . :- . #(Lit e _) #(Reachable e κ))
  (#(Evaluated e e κ) . :- . #(Id e _) #(Reachable e κ))
  (#(Evaluated e e κ) . :- . #(Lam e _ _) #(Reachable e κ))
  (#(Evaluated e-rator e κ) . :- .  #(App e e-rator _) #(Reachable e κ))
  (#(Evaluated e-rand e κ) . :- . #(App e _ e-rands) #(∈1 e-rand e-rands) #(Reachable e κ))
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
  (#(Lookup-root x e-body κ‘ #(root e-rand e κ)) . :- . #(App e e-rator e-rands) #(Lam e-lam e-params e-body)
                                                         #(∈1 e-param i e-params) #(Id e-param x) #(∈3 e-rand i e-rands) #(Step e κ e-body κ‘))
  (#(Lookup-root x e-body κ‘ r) . :- . #(App e e-rator _) #(Geval e-rator e κ #(obj e-lam e-obj κ-obj))
                                        #(Lookup-root x e-obj κ-obj r) (¬ #(Binds e-lam x)) #(Step e κ e-body κ‘))
  (#(Lookup-root "+" e κ #f) . :- . #(Root e) #(Reachable e κ))
  (#(Lookup-root "-" e κ #f) . :- . #(Root e) #(Reachable e κ))
  (#(Lookup-root "*" e κ #f) . :- . #(Root e) #(Reachable e κ))
  (#(Lookup-root "=" e κ #f) . :- . #(Root e) #(Reachable e κ))
  (#(Lookup-root "<" e κ #f) . :- . #(Root e) #(Reachable e κ))
  
  (#(Geval e‘ e κ d) . :- . #(Lit e‘ d) #(Evaluated e‘ e κ))
  (#(Geval e‘ e κ d) . :- . #(Id e‘ x) #(Evaluated e‘ e κ) #(Lookup-root x e κ #(root e-r e-rs κ-rs)) #(Geval e-r e-rs κ-rs d))
  (#(Geval e‘ e κ #(prim proc)) . :- . #(Id e‘ x) #(Evaluated e‘ e κ) #(Lookup-root x e κ #f) #(Prim x proc))
  (#(Geval e‘ e κ #(obj e‘ e κ)) . :- . #(Lam e‘ _ _) #(Evaluated e‘ e κ))
  (#(Geval e e κ d) . :- . #(Let e _ _ e-body) #(Reachable e κ) #(Geval e-body e-body κ d))
  (#(Geval e e κ d) . :- . #(Letrec e _ _ e-body) #(Reachable e κ) #(Geval e-body e-body κ d))
  (#(Geval e e κ d) . :- . #(App e _ _) #(Step e κ e-body κ‘) #(Lam _ _ e-body) #(Geval e-body e-body κ‘ d))
  (#(Geval e e κ d) . :- . #(App e e-rator e-rands) #(Reachable e κ) 
                            #(Geval e-rator e κ #(prim proc)) #(∈3 e1 0 e-rands) #(Geval e1 e κ d1) #(∈3 e2 1 e-rands) #(Geval e2 e κ d2) #(= d ,(proc d1 d2)))
  (#(Geval e e κ d) . :- . #(If e _ _ _) #(Step e κ e-thenelse κ) #(Geval e-body e-thenelse κ d))
  
  (#(Final e κ) . :- . #(Reachable e κ) (¬ #(Step e κ e‘ κ‘)))

  (#(Eval e d) . :- . #(Final e κ) #(Geval e e κ d))  
))


(define (test-rules e expected)
  (let ((result
            (with-handlers ((exn:fail?
                             (lambda (exc) (if (eq? expected 'FAIL)
                                             'FAIL
                                             (begin
                                               (printf "unexpected failure for ~a:\n" e)
                                               (raise exc))))))
              (conc-eval ((make-compiler) e)))))
         (unless (equal? result expected)
           (error (format "wrong result for ~a:\n\texpected ~a\n\tgot      ~a" e expected result)))))

(define (conc-eval e)
  (let ((E (set-union (ast->facts e) (set `#(Prim "+" ,+) `#(Prim "-" ,-) `#(Prim "*" ,*) `#(Prim "=" ,=) `#(Prim "<" ,<)))))
    (printf "~a\n" E)
    (let ((facts (solve-semi-naive P E)))
      (unless (= 1 (length (sequence->list (sequence-filter (lambda (a) (eq? 'Root (atom-name a))) (in-set facts)))))
        (error 'conc-eval "wrong number of Roots"))
      (let ((Eval (sequence->list (sequence-filter (lambda (a) (eq? 'Eval (atom-name a))) (in-set facts)))))
        (if (= (length Eval) 1)
            (vector-ref (car Eval) 2)
            (error 'conc-eval "wrong Eval result: ~a" Eval))))))

(define time-test-start (current-milliseconds))
(test-rules '123 123)
(test-rules '(let ((x 10)) x) 10)
(test-rules '(let ((x 10)) (let ((y 20)) y)) 20)
(test-rules '(let ((x 10)) (let ((y 20)) x)) 10)
(test-rules '(let ((x 10)) (let ((x 20)) x)) 20)
(test-rules '(let ((x 123)) (let ((u (let ((x #f)) "dummy"))) x)) 123)
(test-rules '(let ((x 123)) (let ((u (let ((y "dummy")) (let ((x #f)) "dummy2")))) x)) 123)
(test-rules '(let ((x (let ((z 3)) z))) x) 3)

(test-rules '(let ((f (lambda () 123))) (f)) 123) ; added 
(test-rules '(let ((f (lambda (x) x))) (f 123)) 123) ; added
(test-rules '(let ((x 123)) (let ((f (lambda () x))) (f))) 123)
(test-rules '(let ((x 123)) (let ((f (lambda () x))) (let ((x 999)) (f)))) 123)
(test-rules '(let ((f (lambda (x) (let ((v x)) v)))) (f 123)) 123)
(test-rules '(let ((f (lambda (x) x))) (let ((v (f 999))) v)) 999)
(test-rules '(let ((f (lambda (x) x))) (let ((u (f 1))) (f 2))) 2)

(test-rules '(+ 1 1) 2)
(test-rules '(let ((x (+ 1 1))) x) 2)
(test-rules '(let ((f (lambda () (- 5 3)))) (f)) 2)
(test-rules '(let ((f (lambda (x) (* x x)))) (f 4)) 16)
(test-rules '((lambda (x) (* x x)) 4) 16)
(test-rules '(let ((f (lambda (g) (g 4)))) (f (lambda (x) (* x x)))) 16)
(test-rules '(let ((f (lambda (x) x))) (let ((v (+ 3 9))) v)) 12)

(test-rules '(let ((g (lambda (v) v))) (let ((f (lambda (n) (let ((m (g 123))) (* m n))))) (f 2))) 246)
(test-rules '(let ((f (lambda (y) (let ((x y)) x)))) (let ((z (f "foo"))) (f 1))) 1)
(test-rules '(let ((f (lambda (x) (let ((i (lambda (a) a))) (i x))))) (let ((z1 (f 123))) (let ((z2 (f #t))) z2))) #t)
(test-rules '(let ((f (lambda () (lambda (x) (* x x))))) (let ((g (f))) (g 4))) 16)

(test-rules '(if #t 1 2) 1)
(test-rules '(if #f 1 2) 2)
(test-rules '(if #t (+ 3 5) (- 4 6)) 8)
(test-rules '(if #f (+ 3 5) (- 4 6)) -2)
(test-rules '(if #t (let ((x 1)) x) (let ((x 2)) x)) 1)
(test-rules '(if #f (let ((x 1)) x) (let ((x 2)) x)) 2)
(test-rules '(let ((x (if #t 1 2))) x) 1)
(test-rules '(let ((x (if #f 1 2))) x) 2)
(test-rules '(let ((f (lambda (x) (* x x)))) (let ((v (f 4))) (if v (f 5) (f 6)))) 25)

(test-rules '(let ((f (lambda (x) (lambda (y) x)))) (let ((v (f 123))) (v 999))) 123)
(test-rules '(let ((f (lambda (x) (lambda (x) x)))) (let ((v (f 123))) (v 999))) 999)
(test-rules '(let ((f (lambda (g) (g 678)))) (let ((id (lambda (x) x))) (f id))) 678)
(test-rules '(let ((f (lambda (g x) (g x)))) (let ((id (lambda (x) x))) (f id 789))) 789)
(test-rules '(let ((f (lambda (g) (lambda (x) (g x))))) (let ((sq (lambda (x) (* x x)))) (let ((ff (f sq))) (ff 11)))) 121)
(test-rules '(let ((f (lambda (n) (let ((x n)) (lambda () x))))) (let ((f0 (f 0))) (let ((f1 (f 1))) (let ((u (f1))) (f0))))) 0)

(test-rules '(letrec ((f (lambda (x) (if x "done" (f #t))))) (f #f)) "done")
(test-rules '(letrec ((f (lambda (x) (let ((v (= x 2))) (if v x (let ((u (+ x 1))) (f u))))))) (f 0)) 2)
; ^ 35.1 s unoptimized naive; 10.9 s semi-naive + opti
; (test-rules '(letrec ((count (lambda (n) (let ((t (= n 0))) (if t 123 (let ((u (- n 1))) (let ((v (count u))) v))))))) (count 1)) 123)
; (test-rules '(letrec ((fac (lambda (n) (let ((v (= n 0))) (if v 1 (let ((m (- n 1))) (let ((w (fac m))) (* n w)))))))) (fac 1)) 1)
; (test-rules '(letrec ((fac (lambda (n) (let ((v (= n 0))) (if v 1 (let ((m (- n 1))) (let ((w (fac m))) (* n w)))))))) (fac 3)) 6)
; (test-rules '(letrec ((fib (lambda (n) (let ((c (< n 2))) (if c n (let ((n1 (- n 1))) (let ((n2 (- n 2))) (let ((f1 (fib n1))) (let ((f2 (fib n2))) (+ f1 f2)))))))))) (fib 1)) 1)
; (test-rules '(letrec ((fib (lambda (n) (let ((c (< n 2))) (if c n (let ((n1 (- n 1))) (let ((f1 (fib n1))) (let ((n2 (- n 2))) (let ((f2 (fib n2))) (+ f1 f2)))))))))) (fib 1)) 1)
; (test-rules '(letrec ((fib (lambda (n) (let ((c (< n 2))) (if c n (let ((n1 (- n 1))) (let ((n2 (- n 2))) (let ((f1 (fib n1))) (let ((f2 (fib n2))) (+ f1 f2)))))))))) (fib 3)) 2)
; (test-rules '(letrec ((fib (lambda (n) (let ((c (< n 2))) (if c n (let ((n1 (- n 1))) (let ((f1 (fib n1))) (let ((n2 (- n 2))) (let ((f2 (fib n2))) (+ f1 f2)))))))))) (fib 3)) 2)
; ^ excluded

(test-rules 'x 'FAIL)
(test-rules '(let ((f (lambda () f))) (f)) 'FAIL)
; ^ full: 339.4 s unoptimized naive; 69.6 s semi-naive + opti

; set!
; (test-rules '(let ((g #f)) (let ((f (lambda (n) (let ((x n)) (let ((u (if g 123 (set! g (lambda (y) (set! x y)))))) (lambda () x))))))
;                                (let ((f0 (f 0)))
;                                  (let ((u (g 9)))
;                                    (let ((f1 (f 1)))
;                                      (let ((u (f1)))
;                                        (f0))))))) 9)


; cons car cdr
; (test-machine '(let ((x (if #t (cons 1 2) (cons 3 4)))) (car x)) 1)
; (test-machine '(let ((x (if #t (cons 1 2) (cons 3 4)))) (cdr x)) 2)
; (test-machine '(let ((x (if #f (cons 1 2) (cons 3 4)))) (car x)) 3)
; (test-machine '(let ((x (if #f (cons 1 2) (cons 3 4)))) (cdr x)) 4)


(define time-test-end (current-milliseconds))
(printf "~a ms" (- time-test-end time-test-start))