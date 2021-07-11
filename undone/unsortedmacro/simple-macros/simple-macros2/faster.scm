
; Interpreter with minimal preprocessing.

; Strategy: translate source code to list structure.  Each expression
; becomes a pair ("node") whose car is a small integer index into a
; vector of interpretation procedures.  Local variables become (x .
; name), top-level variables become (x . location) where location
; holds the variable's value (no lookup required), constants become (x
; . value), etc.

; Function is controlled by two parameters:
;   *eager-preprocessing?*
;      #t - preprocess recursively into lambdas
;      #f - don't preprocess until the first time a closure over that
;	    particular lambda expression is called
;   *clean?*
;      #t - don't generate with-aliases nodes for macro expansions 
;	    when it is definitely safe not to
;      #f - always generate with-aliases nodes

; Could be faster if any kind of variable dead-reckoning were performed
; (e.g. depth in a list, or depth + offset in chain of vectors).  Doing
; so would also obviate the need for WITH-ALIASES nodes.

; Requires Scheme 48's define-enumeration macro (whose meaning should
; be obvious), plus a few primitives for doing timings (optional).

(define (fa exp)
  (faster exp top-level-env))

(define (faster exp env)
  (ev-node (prep exp env) env))


(define-enumeration node
  (constant
   variable
   top-level
   application
   variable
   application
   quote
   lambda
   if
   begin
   set!
   let-syntax
   with-aliases))

(define make-node cons)
(define node-type car)
(define node-stuff cdr)

; Preprocessor

(define (prep exp env)			; returns a "node"
  (cond ((name? exp) (prep-variable exp env))
	((pair? exp)
	 (if (name? (car exp))
	     (let ((den (binding env (car exp))))
	       (cond ((special? den)
		      (prep-special-form den exp env))
		     ((macro? den)
		      (prep-macro-application den exp env))
		     (else
		      (prep-application exp env))))
	     (prep-application exp env)))
	(else (prep-constant exp))))

(define (prep-variable exp env)
  (let ((den (binding env exp)))
    (if (location? den)
	(make-node node/top-level den)
	(make-node node/variable exp))))   ;(simplify-name exp env) ?

(define (prep-constant datum)
  (make-node node/constant datum))

(define (prep-macro-application mac exp env-of-use)
  (call-with-values
      (lambda () (transform mac exp env-of-use))
    (lambda (new-exp uid)
      (let ((new-body (prep new-exp (bind-aliases uid mac env-of-use))))
	(if (and *clean?*		;Reliable?
		 (not (contains-generated? new-body uid)))
	    new-body
	    (make-node node/with-aliases
		       `(,mac ,uid ,new-body)))))))

(define *clean?* #f)

(define (set-clean?! flag)
  (set! *clean?* flag))

(define (prep-application exp env)
  (make-node node/application
	     (cons (prep (car exp) env)
		   (map (lambda (arg) (prep arg env))
			(cdr exp)))))

(define (prep-special-form special exp env)
  ((case (special-name special)
     ((quote) prep-quote)
     ((lambda) prep-lambda)
     ((if) prep-if)
     ((begin) prep-begin)
     ((set!) prep-set!)
     ((let-syntax) prep-let-syntax)
     (else (error "unrecognized special operator" special)))
   exp env))

(define (prep-quote exp env)
  (prep-constant (desyntaxify (cadr exp))))

(define *eager-preprocessing?* #f)

(define (prep-lambda exp env)
  (make-node node/lambda
	     (if *eager-preprocessing?*
		 (cons #f (really-prep-lambda (cdr exp) env))
		 ;; (delayed? vars+body . env)
		 (cons #t (cons (cdr exp) env)))))

(define (really-prep-lambda vars+body env) ;happens later
  `(,(car vars+body)
    ,@(prep-sequence (cdr vars+body)
		     (bind-to-random (car vars+body) env))))

(define (prep-if exp env)
  (make-node node/if
    `(,(prep (cadr exp) env)
      ,(prep (caddr exp) env)
      ,(if (null? (cdddr exp))
	   (prep-constant (if #f 0))
	   (prep (cadddr exp) env)))))

(define (prep-begin exp env)
  (make-node node/begin (prep-sequence (cdr exp) env)))

(define (prep-sequence forms env)
  (map (lambda (form) (prep form env)) forms))

(define (prep-set! exp env)
  (make-node node/set!
    `(,(cadr exp)
      ,(prep (caddr exp) env))))

(define (prep-let-syntax exp env)
  (let ((specs (cadr exp))
	(body (cddr exp)))
    (make-node node/let-syntax
      `(,specs
	,@(prep-sequence
	   (cddr exp)
	   (bind (map car specs)
		 (map (lambda (spec)
			(make-macro (eval-transformer (cadr spec) env)
				    env))
		      specs)
		 env))))))

; For preprocessing LAMBDAs:

(define (bind-to-random names env)
  (bind names (map make-random-denotation names) env))




; Interpreter for intermediate code

(define (ev-node node env)			;Main dispatch
  ((vector-ref evaluators (node-type node)) (node-stuff node) env))

(define evaluators (make-vector node-count))

(define (define-evaluator which proc)
  (vector-set! evaluators which proc))

(define-evaluator node/constant (lambda (stuff env) stuff))

(define-evaluator node/variable
  (lambda (stuff env)
    (value (binding env stuff))))

(define-evaluator node/top-level
  (lambda (stuff env)
    (value stuff)))

(define-evaluator node/application
  (lambda (stuff env)
    (apply (ev-node (car stuff) env)
	   (map (lambda (arg) (ev-node arg env))
		(cdr stuff)))))

(define-evaluator node/lambda
  (lambda (stuff env)
    (lambda args
      (if (car stuff)			;(delayed? vars+body . env)
	  (begin (set-cdr! stuff
			   (really-prep-lambda (cadr stuff) (cddr stuff)))
		 (set-car! stuff #f)))
      (let ((args+body (cdr stuff)))
	(ev-node-sequence (cdr args+body) ;Yow.
			  (bind-to-locations (car args+body) args env))))))

(define-evaluator node/if
  (lambda (stuff env)
    (if (ev-node (car stuff) env)
	(ev-node (cadr stuff) env)
	(ev-node (caddr stuff) env))))

(define-evaluator node/begin
  (lambda (stuff env)
    (ev-node-sequence stuff env)))

(define (ev-node-sequence forms env)
  (if (null? (cdr forms))
      (ev-node (car forms) env)
      (begin (ev-node (car forms) env)
	     (ev-node-sequence (cdr forms) env))))

(define-evaluator node/set!
  (lambda (stuff env)
    (set-value! (binding env (car stuff))
		(ev-node (cadr stuff) env))))

(define-evaluator node/let-syntax
  (lambda (stuff env)
    (let ((specs (car stuff))
	  (body (cdr stuff)))
      (ev-node-sequence (cdr stuff)
			(bind (map car specs)
			      (map (lambda (spec)
				     (make-macro '*transformer-not-needed*
						 env))
				   specs)
			      env)))))

(define-evaluator node/with-aliases
  (lambda (stuff env-of-use)
    (let ((mac (car stuff))
	  (uid (cadr stuff))
	  (body (caddr stuff)))
      (ev-node body
	       (bind-aliases uid mac env-of-use)))))



; Bind some variables to fresh locations initialized to the specified
; values.

(define (bind-to-locations names args env)
  (bind names (map make-location args) env))

; copied from ex.scm
(define (simplify-name name env)
  (if (and (generated? name)
	   (same-denotation? (binding env name)
			     (binding env (generated-name name))))
      (generated-name name)
      name))



; Scheme 48-specific timing routines.

(define (tim thunk)
  (let ((start-time (time time-option/run-time #f)))
    (call-with-values thunk
      (lambda results
	(let* ((stop-time (time time-option/run-time #f))
	       (dt (- stop-time start-time))
	       (units-per-second (time time-option/ticks-per-second #f))
	       (delta (quotient (* dt 100) units-per-second))
	       (port (current-output-port)))
	  (display "Run time: " port)
	  (write-hundredths delta port)
	  (display " seconds" port)
	  (newline port)
	  (apply values results))))))

(define time-option/run-time (enum time-option run-time))
(define time-option/ticks-per-second (enum time-option ticks-per-second))

(define (write-hundredths n port)
  (write (quotient n 100) port)
  (write-char #\. port)
  (let ((r (remainder n 100)))
    (if (< r 10)
	(write-char #\0 port))
    (write r port)))


(define (timings)
  (time3 (lambda () (load "loser.scm")))
  (let ((try (lambda (eager? clean?)
	       (set! *eager-preprocessing?* eager?)
	       (set! *clean?* clean?)
	       (display (if *eager-preprocessing?* "Eager, " "Lazy, "))
	       (display (if *clean?* "clean:" "not clean:"))
	       (newline)
	       (display " - close") (newline)
	       (time3 (lambda () (fa loser)))
	       (display " - first") (newline)
	       (let ((w
		      (let ((z (lambda () (let ((w (fa loser)))
					    (tim (lambda () (w 1)))
					    w))))
			(z) (z) (z))))
		 (display " - subsequent") (newline)
		 (time3 (lambda () (w 1)))
		 (newline)))))
    (try #f #f)
    (try #f #t)
    (try #t #f)
    (try #t #t)))

(define (time3 x) (tim x) (tim x) (tim x))
