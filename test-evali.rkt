#lang racket

(require "ast.rkt")
(require "evali.rkt")

(define (create-timer)
  (let ((start (current-milliseconds)))
    (lambda ()
      (- (current-milliseconds) start))))

(define compile (make-compiler))

(define tests 0)
(define errors 0)
(define warning-incremental-slower 0)
(define results '())

(define (report-error e expected actual)
  (set! errors (add1 errors))
  (printf "error: wrong result; expected ~a got ~a\nprogram: ~a\n" expected actual e))

(define (report-incremental-slower-warning e)
  (set! warning-incremental-slower (add1 warning-incremental-slower)))

; timings:
; - in favor of delta: prov information computed which is not needed  
; - in favor of from-scratch: piggy-backs on ast rewriting from evali

(define (test-change-literal-value e e-lit d)
  (set! tests (add1 tests))
  (define ii (evali e))

  (printf "p:  ~a\n" (ast->string (ii 'program)))

  (define incremental-timer (create-timer))
  (define ii‘ (ii 'change-literal-value e-lit d))
  (define incremental-time (incremental-timer))

  (printf "p‘:  ~a\n" (ast->string (ii‘ 'program)))

  (define actual (ii‘ 'tuples))
  (define e‘ (ii‘ 'program))

  (define from-scratch-timer (create-timer))
  (define ii‘-scratch (evali e‘))
  (define from-scratch-time (from-scratch-timer))

  (define expected (ii‘-scratch 'tuples))

  (unless (equal? actual expected)
    (report-error e‘ expected actual))

  (set! results (append results (list (hash 'from-scratch-time from-scratch-time 'incremental-time incremental-time))))

  (printf "from-scratch: ~a ms; incremental: ~a ms\n\n" from-scratch-time incremental-time)

  (unless (<= incremental-time from-scratch-time)
    (report-incremental-slower-warning e)))

(let ((e-lit (compile #t))
      (d #f))
  (let ((e (compile
    `(let ((x ,e-lit))
      (if x
          'neg
          'zeropos)))))
    (test-change-literal-value e e-lit d)))

(let ((e-lit (compile 2))
      (d 3))
  (let ((e (compile
    `(let ((x ,e-lit))
      (let ((f (lambda (y) (* y y))))
        (f x))))))
    (test-change-literal-value e e-lit d)))

(let ((e-lit (compile 2))
      (d 3))
  (let ((e (compile
    `(let ((x ,e-lit))
      (let ((c (< x 0)))
        (if c
            'neg
            'zeropos))))))
    (test-change-literal-value e e-lit d)))

(let ((e-lit (compile 2))
      (d 4))
  (let ((e (compile
    `(let ((x ,e-lit))
      (letrec ((f
        (lambda (x)
          (let ((c (< x 2)))
            (if c
                1
                (let ((n (- x 1)))
                  (let ((m (f n)))
                    (* x m))))))))
        (f x))))))
    (test-change-literal-value e e-lit d)))

(let ((e-lit (compile 4))
      (d 2))
  (let ((e (compile
    `(let ((x ,e-lit))
      (letrec ((f
        (lambda (x)
          (let ((c (< x 2)))
            (if c
                1
                (let ((n (- x 1)))
                  (let ((m (f n)))
                    (* x m))))))))
        (f x))))))
    (test-change-literal-value e e-lit d)))

(let ((e-lit (compile 4))
      (d 2))
  (let ((e (compile
    `(letrec ((f
      (lambda (x)
        (let ((c (< x 2)))
          (if c
              1
              (let ((n (- x 1)))
                (let ((m (f n)))
                  (* x m))))))))
      (let ((u (f 4)))
        (let ((y ,e-lit))
          y))))))
    (test-change-literal-value e e-lit d)))

(let ((e-lit (compile 4))
      (d 2))
  (let ((e (compile
    `(letrec ((f
      (lambda (x)
        (let ((c (< x 2)))
          (if c
              1
              (let ((n (- x 1)))
                (let ((m (f n)))
                  (* x m))))))))
      (let ((y ,e-lit))
        (let ((u (f 4)))
          y))))))
    (test-change-literal-value e e-lit d)))


(printf "\n\nTESTS: ~a\n" tests)
(for ((result results))
  (let* ((fst (hash-ref result 'from-scratch-time))
          (it (hash-ref result 'incremental-time))
          (x (/ fst it)))
    (printf "~a ~a ~ax\n" (~a fst #:min-width 8) (~a it #:min-width 8) (~r x #:precision 1))))
(printf "WARNINGS: incr slower ~a\n"
  warning-incremental-slower)
(printf "ERRORS: ~a\n\n"
  errors)