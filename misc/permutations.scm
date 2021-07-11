
(define (permutations items)
;;
    (define (remove x lst)
    (cond
     ((null? lst) '())
     ((eqv? x (car lst))(remove x (cdr lst)))
     (else (cons (car lst) (remove x (cdr lst))))))
;;    
    (if (null? items) '(())
      (apply append
             (map (lambda (element)
            (map (lambda (permutation)
               (cons element permutation))
             (permutations (remove element items))))
          items))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lst '(1 2 3 4))

(display (permutations lst))

;;;;old
#|
(define (permute2 lst)
  (define (remove x lst)
    (cond
     ((null? lst) '())
     ((eqv? x (car lst))(remove x (cdr lst)))
     (else (cons (car lst) (remove x (cdr lst))))))
  (cond
   ((= (length lst) 1)(list lst))
   (else (apply append (map (lambda (i)
                      (lambda (j) (cons i j))
                      (permute2 (remove i lst)))
                    lst)))))
|#
