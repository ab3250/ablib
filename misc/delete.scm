(define delete
  (case-lambda
    ((x lis)
     (delete x lis equal?))
    ((x lis elt=)
     (let recur ((lis lis))
       (if (eqv? lis '()) lis
           (let ((head (car lis))
                 (tail (cdr lis)))
             (if (not (elt= x head))
                 (let ((new-tail (recur tail)))
                   (if (eq? tail new-tail) lis
                       (cons head new-tail)))
                 (recur tail))))))))
