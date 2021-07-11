(define for-all andmap)

(define (andmap f xs)
    (cond ((null? xs) #t)
          ((f (car xs))
            (andmap f (cdr xs)))
          (else #f)))

;;(andmap even? '(2 4 6 8 10)) => #t
;;(andmap even? '(2 4 5 6 8)) => #f

(define (ormap f xs)
    (cond ((null? xs) #f)
          ((f (car xs)) #t)
          (else (ormap f (cdr xs)))))

;;(ormap odd? '(2 4 6 8 10)) => #f
;;(ormap odd? '(2 4 5 6 8)) => #t
