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
            (loop (set-union (set-rest W) (set x e0 e1)) (set-add E `#(Letrec ,l (ast-label x) ,(ast-label e0) ,(ast-label e1)))))
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
          ((«app» l rator rands)
            (loop (set-union (set-rest W) (set rator) (list->set rands)) (set-add E `#(App l (map ast-label e-params) (ast-label e-body)))))
          (_ (error "ast->facts: cannot handle expression" e))))))

(define P (set

  (#(Parent e p) . --> . #(Let p e _ _))
  (#(Parent e p) . --> . #(Let p _ e _))
  (#(Parent e p) . --> . #(Let p _ _ e))
  (#(Parent e p) . --> . #(Letrec p e _ _))
  (#(Parent e p) . --> . #(Letrec p _ e _))
  (#(Parent e p) . --> . #(Letrec p _ _ e))

  (#(Eval e d) . --> . #(Final e κ) #(Geval e e κ d))
  
  (#(Final e κ) . --> . #(Reachable e κ) (¬ #(Step e κ e‘ κ‘)))

  (#(Geval e e‘ κ‘ d) . --> . #(Reachable e‘ κ‘) #(Lit e d))
  (#(Geval e e‘ κ‘ d) . --> . #(Reachable e‘ κ‘) #(Id e x) #(Eval-var-root e‘ κ‘ d))

  (#(Reachable e‘ κ‘) . --> . #(Reachable e κ) #(Step e κ e‘ κ‘))
  
  (#(Step e κ e‘ κ‘) . --> . #(Reachable e κ) #(Lit e d) #(Cont e κ e‘ κ‘))
  (#(Step e κ e‘ κ‘) . --> . #(Reachable e κ) #(Id e d) #(Cont e κ e‘ κ‘))
  (#(Step e κ e-init κ) . --> . #(Reachable e κ) #(Let e e-id e-init e-body))
  (#(Step e κ e-init κ) . --> . #(Reachable e κ) #(Letrec e e-id e-init e-body))

  (#(Cont e κ e-body κ) . --> . #(Reachable e κ) #(Parent e p) #(Let p _ e e-body))
  (#(Cont e κ e-body κ) . --> . #(Reachable e κ) #(Parent e p) #(Letrec p _ e e-body))

  (#(Eval-var-root e-rs κ-rs d) . --> . #(Lookup-var-root x e κ #(root e-r e-rs κ-rs)) #(Geval e-r e-rs κ-rs d))

  (#(Lookup-var-root x e κ #(root e-init e-init κ)) . --> . #(Reachable e κ) #(Parent e p) #(Let p e-id e-init _) #(Id e-id x) (¬ #(Let p _ e _)))

  
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
  (let ((E (set-add (ast->facts e) #(Reachable 0 0))))
    (printf "~a\n" E)
    (let ((facts (solve-naive P E)))
      (let ((Eval (sequence->list (sequence-filter (lambda (a) (eq? 'Eval (atom-name a))) (in-set facts)))))
        (if (= (length Eval) 1)
            (vector-ref (car Eval) 2)
            (error 'conc-eval "wrong Eval result: ~a" Eval))))))

; (test-rules '123 123)
; (test-rules '(let ((x 10)) x) 10)
(test-machine '(let ((x 10)) (let ((y 20)) y)) 20)
