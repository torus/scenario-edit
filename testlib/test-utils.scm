(define (assert-equal a b)
  (test* "equality check" a b))

(define (test-behavior comment proc)
  (test-section comment)
  (proc)
  )

(define (id x) x)
(define (fail x) 'fail)
