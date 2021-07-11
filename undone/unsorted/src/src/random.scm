;; Random numbers using linear congruential method.
;; Copyright Juha Heinanen 1988
;; This code may be freely distributed.
;;

  (define (rand-update x)
    (define a 25173)
    (define b 13849)
    (define m 65536)
    (modulo (+ (* a x) b) m))

  (define rand
    (let ((x 1))
      (lambda ()
        (set! x (rand-update x))
        x)))

  (define (random n)
    (modulo (rand) n))
