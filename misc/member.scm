;;; Extended from R4RS to take an optional comparison argument.
(define member
  (case-lambda
    ((x lis)
     (r6rs:member x lis))
    ((x lis elt=)
     (let lp ((lis lis))
       (and (not (eqv? lis '()))
            (if (elt= x (car lis)) lis
                (lp (cdr lis))))))))
