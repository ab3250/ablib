
(require-extension (srfi 10000))

(common-module (math fibonacci)
  ((export fib))

(define (fib n)
  (if (< n 2)
    1
    (+ (fib (- n 1)) (fib (- n 2)))))

)

