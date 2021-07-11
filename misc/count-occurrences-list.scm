
(define (count2 x L)
  (if (null? L)
      0
      (if (eq? x (car L))
	  (+ 1 (count2 x (cdr L)))
	  (count2 x (cdr L)))))

(define (len L)
  (if (null? L)
      0
      (+ 1 (len (cdr L)))))
