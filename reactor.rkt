(define tempr
  (reactor
    '(let ((c2f
        (lambda (t)
          (+ (* t 1.8) 32))))
      (let ((c2k
        (lambda (t)
            (+ t 273.15))))
        (let ((convert
          (lambda (t)
            (let ((f (c2f t)))
              (let ((k ((c2k t))
                (cons f k))))))))
        (convert t))))))

(tempr `(t 5))
(tempr `(t 8))
(tempr `(t 6))

(define sumr
  (reactor
    '(let ((sum 0))
      (let ((adder
          (lambda (n)
            (set! sum (+ sum n))
            sum)))
        (adder n)))))

(define countr
  (reactor
    '(let ((count 0))
      (let ((counter
          (lambda (_)
            (set! count (+ count 1))
              count)))
        (counter _)))))

(define avgr
  (reactor
    '(/ s c)))

(avgr `(s 10 ))



