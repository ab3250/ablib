; Macro expansion.

; Doesn't support rebinding the name WITH-ALIASES.

(define (expand exp env)
  (cond ((name? exp) (expand-name exp env))
	((pair? exp)
	 (if (name? (car exp))
	     (let ((den (binding env (car exp))))
	       (cond ((special? den)
		      (expand-special-form den exp env))
		     ((macro? den)
		      (expand-macro-application den exp env))
		     (else
		      (expand-application exp env))))
	     (expand-application exp env)))
	(else exp)))

(define (expand-macro-application mac exp env-of-use)
  (call-with-values
      (lambda () (transform mac exp env-of-use))
    (lambda (new-exp uid)
      (let ((new-body (expand new-exp (bind-aliases uid mac env-of-use))))
	(if (and *clean?*
		 (not (contains-generated? new-body uid)))
	    new-body
	    `(with-aliases ,(expand-name (car exp) env-of-use) ,uid
	       ,new-body))))))

(define (expand-application exp env)
  (cons (expand (car exp) env)
	(map (lambda (arg) (expand arg env))
	     (cdr exp))))

(define (expand-special-form special exp env)
  ((case (special-name special)
     ((quote) expand-quote)
     ((lambda) expand-lambda)
     ((if) expand-if)
     ((begin) expand-begin)
     ((set!) expand-set!)
     ((define) expand-define)
     ((define-syntax) expand-define-syntax)
     ((let-syntax) expand-let-syntax)
     ((with-aliases) expand-with-aliases)
     (else (error "unrecognized special operator" special)))
   exp env))

(define (expand-quote exp env)
  (if *clean?*
      (form-special exp env `(,(desyntaxify (cadr exp))))
      exp))  ;will get fixed later by EVAL

(define (expand-lambda exp env)
  (form-special exp env
    `(,(cadr exp)
      ,@(expand-sequence (cddr exp)
			 (bind-to-random (cadr exp) env)))))

(define (expand-if exp env)
  (form-special exp env
    `(,(expand (cadr exp) env)
      ,(expand (caddr exp) env)
      ,@(if (null? (cdddr exp))
	    `()
	    `(,(expand (cadddr exp) env))))))

(define (expand-begin exp env)
  (form-special exp env (expand-sequence (cdr exp) env)))

(define (expand-sequence forms env)
  (map (lambda (form) (expand form env)) forms))

(define (expand-set! exp env)
  (form-special exp env
    `(,(expand-name (cadr exp) env)
      ,(expand (caddr exp) env))))

(define (expand-define exp env)
  (call-with-values (lambda () (parse-define exp))
    (lambda (lhs rhs)
      (form-special exp env
	`(,lhs ,(expand rhs env))))))

(define (expand-define-syntax exp env)
  (define-top-level! (cadr exp)
    (make-macro (eval-transformer (caddr exp) env)
		env))
  exp)

(define (expand-let-syntax exp env)
  (let ((specs (cadr exp))
	(body (cddr exp)))
    (form-special exp env
      `(,specs
	,@(expand-sequence
	   (cddr exp)
	   (bind (map car specs)
		 (map (lambda (spec)
			(make-macro (eval-transformer (cadr spec) env)
				    env))
		      specs)
		 env))))))

(define (expand-with-aliases exp env-of-use)
  (let ((keyword (cadr exp))
	(uid (caddr exp))
	(body (cadddr exp)))
    (let ((mac (binding env-of-use keyword)))
      (if (not (macro? mac))
	  (error "macro name required here" exp))
      (form-special exp env-of-use
	`(,keyword ,uid
		   ,(expand body
			    (bind-aliases uid mac env-of-use)))))))

(define (form-special exp env tail)
  `(,(expand-name (car exp) env) ,@tail))

(define *clean?* #t)

(define (set-clean?! flag)
  (set! *clean?* flag))

(define (expand-name name env)
  (if (and *clean?*
	   (generated? name)
	   (same-denotation? (binding env name)
			     (binding env (generated-name name))))
      (generated-name name)
      name))


; For expanding LAMBDAs:

(define (bind-to-random names env)
  (bind names (map make-random-denotation names) env))
