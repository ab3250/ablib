
(define naive-env
  (bind '()
	'()
	top-level-env))

(define (eval-naive exp env)
  (ev `(lambda (expression rename compare)
	 (let ((eq? compare))
	   (let-syntax ((quote (syntax-rules ()
				 ((quote x) (rename (quote x))))))
	     (let-syntax ((quasiquote
			   (transformer
			    (lambda (e r c)
			      (rewrite/quasiquote r c (cadr e))))))
	       (,exp expression)))))
      naive-env))

