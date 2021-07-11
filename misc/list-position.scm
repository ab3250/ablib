(define position

  (define add-1-if-number
    (lambda (val)
      (if (number? val)
          (+ val 1)
          val)))

  (lambda (val li)
    (cond ((null? li) #f)
          ((equal? val (car li)) 0)
          (else (add-1-if-number (position val (cdr li)))))))

-------or


(define list-position
  (lambda (o l)
    (let loop ((i 0) (l l))
      (if (null? l) #f
          (if (eqv? (car l) o) i
              (loop (+ i 1) (cdr l)))))))
