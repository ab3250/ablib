;;;;;do
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
;;;;
