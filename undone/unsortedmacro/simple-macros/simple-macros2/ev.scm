; Evaluator

(define (ev exp env)			;Main dispatch
  (cond ((name? exp) 
	 (value (binding env exp)))
	((pair? exp)
	 (let ((exp (memo-perhaps exp)))
	   (if (name? (car exp))
	       (let ((den (binding env (car exp))))
		 (cond ((special? den)
			(ev-special-form den exp env))
		       ((macro? den)
			(ev-macro-application den exp env))
		       (else
			(ev-application exp env))))
	       (ev-application exp env)))
	 (else exp))))

(define (ev-macro-application mac exp env-of-use)
  (call-with-values
      (lambda () (transform mac exp env-of-use))
    (lambda (new-exp uid)
      (*memoizer* exp
		  `(with-aliases ,(car exp) ,uid ,new-exp)) ;nonhygienic
      (ev new-exp (bind-aliases uid mac env-of-use)))))

(define *memoizer* (lambda (old new) #f))  ;No-op.  See memo.txt
(define (set-memoizer! proc) (set! *memoizer* proc))

(define (ev-application exp env)
  (apply (ev (car exp) env)
	 (map (lambda (arg) (ev arg env))
	      (cdr exp))))

(define (ev-special-form special exp env)
  ((case (special-name special)
     ((quote) ev-quote)
     ((lambda) ev-lambda)
     ((if) ev-if)
     ((begin) ev-begin)
     ((set!) ev-set!)
     ((define) ev-define)
     ((define-syntax) ev-define-syntax)
     ((let-syntax) ev-let-syntax)
     ((with-aliases) ev-with-aliases)
     (else (error "unrecognized special operator" special)))
   exp env))

(define (ev-quote exp env)
  (let ((datum (desyntaxify (cadr exp))))
    (if (eq? datum (cadr exp))
	datum
	;; Memoization is necessary in order to ensure that every
	;; evaluation of a given quotation will be EQ? to every other
	;; evaluation of that quotation.
	(begin (memoize! exp `(,(car exp) ,datum))
	       datum))))

(define (ev-lambda exp env)
  (lambda args
    (ev-sequence (cddr exp)
		 (bind-to-locations (cadr exp) args env))))

(define (ev-if exp env)
  (if (ev (cadr exp) env)
      (ev (caddr exp) env)
      (if (not (null? (cdddr exp)))
	  (ev (cadddr exp) env))))

(define (ev-begin exp env)
  (ev-sequence (cdr exp) env))

(define (ev-sequence forms env)
  (if (null? (cdr forms))
      (ev (car forms) env)
      (begin (ev (car forms) env)
	     (ev-sequence (cdr forms) env))))

(define (ev-set! exp env)
  (set-value! (binding env (cadr exp))
	      (ev (caddr exp) env)))

(define (ev-define exp env)
  (call-with-values (lambda () (parse-define exp))
    (lambda (lhs rhs)
      (define-top-level! lhs
	(make-location rhs)))))

(define (ev-define-syntax exp env)
  (define-top-level! (cadr exp)
    (make-macro (eval-transformer (caddr exp) env)
		env)))

(define (ev-let-syntax exp env)
  (let ((specs (cadr exp))
	(body (cddr exp)))
    (ev-sequence (cddr exp)
		 (bind (map car specs)
		       (map (lambda (spec)
			      (make-macro (eval-transformer (cadr spec) env)
					  env))
			    specs)
		       env))))

(define (ev-with-aliases exp env-of-use)
  (let ((keyword (cadr exp))
	(uid (caddr exp))
	(body (cadddr exp)))
    (let ((mac (binding env-of-use keyword)))
      (if (not (macro? mac))
	  (error "macro name required here" exp))
      (ev body
	  (bind-aliases uid mac env-of-use)))))

; Bind some variables to fresh locations initialized to the specified
; values.

(define (bind-to-locations names args env)
  (bind names (map make-location args) env))
