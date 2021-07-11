(define (remv lst num)
  ; removes the first occurence of num from list lst
  (let loop ((res '()) (lst lst))
    (if (null? lst)
        (reverse res)
        (if (= (car lst) num)
            (append (reverse res) (cdr lst))
            (loop (cons (car lst) res) (cdr lst))))))

(define (enum min max)
  ; creates a list containing integers from min to max
  (let loop ((res '()) (i max))
    (if (< i min)
        res
        (loop (cons i res) (- i 1)))))
  