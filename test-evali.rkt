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

(define (report-error e expected actual)
  (set! errors (add1 errors))
  (printf "error: wrong result; expected ~a got ~a\nprogram: ~a\n" expected actual e))

(define (report-incremental-slower-warning e)
  (set! warning-incremental-slower (add1 warning-incremental-slower)))

; timings:
; - in favor of delta: prov information computed which is not needed  
; - in favor of from-scratch: piggy-backs on ast rewriting from evali

(define (test-replace e e1 e2)
  (set! tests (add1 tests))
  (define ii (evali e))

  (printf "p:  ~a\n" (ast->string (ii 'program)))

  (define incremental-timer (create-timer))
  (define ii‘ (ii 'replace e1 e2))
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

  (printf "from-scratch: ~a ms; incremental: ~a ms\n\n" from-scratch-time incremental-time)

  (unless (<= incremental-time from-scratch-time)
    (report-incremental-slower-warning e)))

(let ((e1 (compile #t))
      (e2 (compile #f)))
  (let ((e (compile
    `(let ((x ,e1))
      (if x
          'neg
          'zeropos)))))
    (test-replace e e1 e2)))

(let ((e1 (compile 2))
      (e2 (compile 3)))
  (let ((e (compile
    `(let ((x ,e1))
      (let ((f (lambda (y) (* y y))))
        (f x))))))
    (test-replace e e1 e2)))

(let ((e1 (compile 2))
      (e2 (compile 3)))
  (let ((e (compile
    `(let ((x ,e1))
      (let ((c (< x 0)))
        (if c
            'neg
            'zeropos))))))
    (test-replace e e1 e2)))


(let ((e1 (compile 2))
      (e2 (compile 4)))
  (let ((e (compile
    `(let ((x ,e1))
      (letrec ((f
        (lambda (x)
          (let ((c (< x 2)))
            (if c
                1
                (let ((n (- x 1)))
                  (let ((m (f n)))
                    (* x m))))))))
        (f x))))))
    (test-replace e e1 e2)))

(let ((e1 (compile 4))
      (e2 (compile 2)))
  (let ((e (compile
    `(let ((x ,e1))
      (letrec ((f
        (lambda (x)
          (let ((c (< x 2)))
            (if c
                1
                (let ((n (- x 1)))
                  (let ((m (f n)))
                    (* x m))))))))
        (f x))))))
    (test-replace e e1 e2)))

(let ((e1 (compile 4))
      (e2 (compile 2)))
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
        (let ((y ,e1))
          y))))))
    (test-replace e e1 e2)))


(printf "\n\nTESTS: ~a\n" tests)
(printf "WARNINGS: incr slower ~a\n"
  warning-incremental-slower)
(printf "ERRORS: ~a\n\n"
  errors)