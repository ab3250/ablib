(define-macro let
  (lambda (bindings . body)
    `((lambda ,(map car bindings) ,@body) 
      ,@(map cadr bindings))))

(define-macro let*
  (lambda (bindings . body)
    (if (null? bindings)
	`((lambda () ,@body))
	`((lambda (,(caar bindings))
	    (let* ,(cdr bindings) ,@body))
	  ,(cadr (car bindings))))))

(define-macro letrec
  (lambda (bindings . body)
    `(let ,(map (lambda (x) (list (car x) ())) 
		bindings)
	  ,@(map (lambda (x) `(set! ,(car x) ,(cadr x))) 
		 bindings)
	  ,@body)))

(define-macro cond
  (lambda args
    (if (null? args)
        '(void)
        (if (null? (cdr args))
            `(if ,(car (car args))
                 (begin ,@(cdr (car args)))
                 ,(void))
            (if (eq? (car (cadr args)) 'else)
                `(if ,(car (car args))
                     (begin ,@(cdr (car args)))
                     (begin ,@(cdr (cadr args))))
                `(if ,(car (car args))
                     (begin ,@(cdr (car args)))
                     (cond ,@(cdr args))))))))

(define-macro do
  (lambda (vars final . body)
    (let ((loop (gensym)))
      `(letrec
	 ((,loop
	   (lambda ,(map car vars)
	     (if ,(car final)
		 ,(if (null? (cdr final))
		      `(void)
		      (cadr final))
		 (begin
		   ,@body
		   (,loop ,@(map caddr vars)))))))
	 (,loop ,@(map cadr vars))))))
