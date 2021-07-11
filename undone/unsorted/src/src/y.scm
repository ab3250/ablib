(define y (lambda (f)
	    (let ((g (lambda (h)
		       (lambda (x)
			 ((f (h h)) x)))))
	      (g g))))

(let ((f (y (lambda (h)
	      (lambda (n)
		(if (< n 2)
		    1
		    (* n (h (- n 1)))))))))
  (f 10))

(let loop ((n 10) (m 1))
  (if (< n 2)
      m
      (loop (- n 1) (* m n))))

(let ((loop (lambda (r)
	      (lambda (n m)
		(if (< n 2)
		    m
		    ((r r) (- n 1) (* m n)))))))
  ((loop loop) 10 1))
