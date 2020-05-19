#lang racket

(require "ast.rkt")
(require "datalog.rkt")
(require "naive.rkt")
(require "semi-naive.rkt")

(define P
  (set
    ; stream semantics

    ; bollekes semantiek
    (:- #(Node node) #(Operator node _))
    
    (:- #(Data node value-out tag) #(Next node value-in tag) #(Operator node lam) #(= value-out (lam value-in)))
    (:- #(Data node value-out tag) #(Next node value-in tag) #(State no)
    

    ; propagation
    (:- #(Next node value tag) #(In node value tag))
    (:- #(Next node value tag) #(Data node‘ value tag) #(Link node‘ node))

    (:- #(NonSink node) #(Link node _))
    (:- #(Sink node) #(Node node) (¬ #(NonSink node)))
    (:- #(Out node value tag) #(Sink node) #(Data node value tag))

  ))

(define E
  (set
    ; graph
    `#(Operator 'op1 ,(lambda (x) (* x x)))
    ;`#(Operator 'op2 ,(lambda (x) (+ x x)))
    `#(Accumulator 'acc1 ,(lambda acc x) (+ acc x))

    #(Link 'op1 'op2)

    ; run
    #(In 'op1 1 'a)
    #(In 'op1 2 'b)
    #(In 'op1 2 'c)
    #(In 'op1 3 'd)

  ))

(match-let (((solver-result tuples num-derived-tuples* solver) (solve-naive P E)))
  ;(printf "RESULT: ~a\n" tuples)
  (printf "~a\n" (solver 'match-atom #(Out _ _ _)))
  )
