;;; 
;;;   Copyright (c) 1993 by Antoine Dumesnil de Maricourt. All rights reserved.
;;; 
;;;   This program is distributed in the hope that it will be useful.
;;;   Use and copying of this software and preparation of derivative works
;;;   based upon this software are permitted, so long as the following
;;;   conditions are met:
;;;        o credit to the authors is acknowledged following current
;;;          academic behaviour
;;;        o no fees or compensation are charged for use, copies, or
;;;          access to this software
;;;        o this copyright notice is included intact.
;;;   This software is made available AS IS, and no warranty is made about 
;;;   the software or its performance. 
;;;  
;;;   Bug descriptions, use reports, comments or suggestions are welcome.
;;;   Send them to    dumesnil@etca.fr   or to:
;;;        
;;;        Antoine de Maricourt
;;;        ETCA CREA-SP
;;;        16 bis, avenue Prieur de la Cote d'Or
;;;        94114 Arcueil Cedex
;;;        France
;;;

(define define-syntax 
  (make-transformer
   (lambda (e)
     (list (syntax define)
	   (car (unwrap-syntax (cdr (unwrap-syntax e))))
	   (cons (syntax make-transformer)
		 (cdr (unwrap-syntax (cdr (unwrap-syntax e))))
		 )))))

(define unwrap-exp 
  (lambda (exp)
    ((lambda (exp)
       (if (pair? exp)
	   (cons (car exp) (unwrap-exp (cdr exp)))
	   exp))
     (unwrap-syntax exp))))

(define-syntax let
  (lambda (e)
    ((lambda (args)
       ((lambda (bds)
	  (cons (cons (syntax lambda)
		      (cons (map (lambda (bd)
				   (car (unwrap-syntax bd)))
				 bds)
			    (cdr args)))
		(map (lambda (bd)
		       (car (unwrap-syntax (cdr (unwrap-syntax bd)))))
		     bds)))
	(unwrap-exp (car args))))
     (cdr (unwrap-exp e)))))

(define-syntax let-syntax
  (lambda (e)
    ((lambda (args)
       ((lambda (bds)
	  (cons (cons (syntax lambda)
		      (cons (map (lambda (bd)
				   (car (unwrap-syntax bd)))
				 bds)
			    (cdr args)))
		(map (lambda (bd)
		       (list (syntax make-transformer)
			     (car (unwrap-syntax (cdr (unwrap-syntax bd))))))
		     bds)))
	(unwrap-exp (car args))))
     (cdr (unwrap-exp e)))))

(define-syntax let*
  (lambda (e)
    (let ((args (cdr (unwrap-exp e))))
      (if (null? (car args))
	  (cons (syntax begin)
		(cdr args))
	  (list (syntax let)
		(list (car (unwrap-syntax (car args))))
		(cons (syntax let*)
		      (cons (cdr (unwrap-syntax (car args)))
			    (cdr args))))
	  ))))

;;;     
;;;  Examples from the paper
;;;

;;;
;;;  (let ((tmp #f))
;;;    (or tmp tmp tmp))
;;;

(define-syntax or
  (lambda (e)
    (let ((args (unwrap-syntax (cdr (unwrap-syntax e)))))
      (if (not (pair? args))
	  #f
	  (if (null? (unwrap-syntax (cdr args)))
	      (car args)
	      (list (syntax let)
		    (list (list (syntax tmp) (car args)))
		    (list
		     (syntax if) (syntax tmp) (syntax tmp)
		     (cons (syntax or)
			   (unwrap-syntax (cdr args)))))
	      )))))

;;;
;;; (let ((* +))
;;;   (fact 5))
;;;

(define-syntax fact 
  (lambda (e)
    (let* ((args (cdr (unwrap-exp e)))
	   (n (car args))
	   (vars (cdr args)))
      (if (= n 0)
	  (cons (syntax *) (cons 1 vars))
	  (list (syntax let)
		(list (list (syntax x) n))
		(cons (syntax fact)
		      (cons (- n 1)
			    (cons (syntax x)
				  vars))))
	  ))))
			    
		 
