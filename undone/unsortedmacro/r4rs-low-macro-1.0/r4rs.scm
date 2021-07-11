
;; R4RS examples

(symbol? (syntax x))

(let-syntax ((car (lambda (x) (syntax car))))
  ((car) '(0)))

(let-syntax ((quote-quote 
	      (lambda (x) 
		(list (syntax quote) 'quote))))
  (quote-quote))

(let-syntax ((quote-quote 
	      (lambda (x) 
		(list 'quote 'quote))))
  (quote-quote))

(let-syntax ((quote-me 
	      (lambda (x) 
		(list (syntax quote) x))))
  (quote-me please))

(let ((x 0))
  (let-syntax ((alpha (lambda (e) (syntax x))))
    (alpha)))

(let ((x 0))
  (let-syntax ((alpha (lambda (x) (syntax x))))
    (alpha)))

(let-syntax ((alpha 
	      (let-syntax ((beta
			    (syntax-rules ()
			      ((beta) 0))))
		(lambda (x) (syntax (beta))))))
  (alpha))

(let-syntax ((alpha (syntax-rules ()
		      ((alpha) 0))))
  (let-syntax ((beta (lambda (x) (alpha))))
    (beta)))

(let ((list 0))
  (let-syntax ((alpha (lambda (x) (list 0))))
    (alpha)))

(identifier? (syntax x))
(identifier? (quote  x))
(identifier? 3)

(identifier? (unwrap-syntax (syntax x)))
(identifier? (car (unwrap-syntax (syntax (x)))))
(unwrap-syntax (cdr (unwrap-syntax (syntax (x)))))

(free-identifier=? (syntax x) (syntax x))
(free-identifier=? (syntax x) (syntax y))

(let ((x (syntax x)))
  (free-identifier=? x (syntax x)))

(let-syntax ((alpha 
	      (lambda (x) 
		(free-identifier=? (car (unwrap-syntax x))
				   (syntax alpha)))))
  (alpha))

(letrec-syntax ((alpha 
		 (lambda (x) 
		   (free-identifier=? (car (unwrap-syntax x))
				      (syntax alpha)))))
  (alpha))

(bound-identifier=? (syntax x) (syntax x))

(letrec-syntax ((alpha 
		 (lambda (x) 
		   (bound-identifier=? (car (unwrap-syntax x))
				       (syntax alpha)))))
  (alpha))

(symbol? (identifier->symbol (syntax x)))
(identifier->symbol (syntax x))

(identifier->symbol (generate-identifier 'x))

(bound-identifier=? (generate-identifier 'x) (generate-identifier 'x))
