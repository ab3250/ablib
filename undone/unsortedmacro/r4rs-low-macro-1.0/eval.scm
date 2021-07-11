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

;;;
;;;  Evaluator
;;;

(define trace-expand		0)
(define trace-expand-level	1)

(define (%eval exp env)

;  (display "eval: ") (%display exp) (newline)

  (cond

   ((and (syntactic-object? exp)
	 (= (env-number env) (syntactic-number exp)))
    (%eval (syntactic-datum exp) env))
   
   ((%identifier? exp) (%value env exp))

   ((syntactic-object? exp)
    (%eval (unwrap-exp exp) env))

   ((pair? exp)

    (let ((exp (unwrap-exp exp)))
      (cond

       ((and (%identifier? (car exp)) (%free-identifier=? (car exp) 'quote))
	(%quote (cadr exp)))
        
       ((and (%identifier? (car exp)) (%free-identifier=? (car exp) 'if))
	(if (%eval (cadr   exp) env)
	    (%eval (caddr  exp) env)
	    (%eval (cadddr exp) env)))
       
       ((and (%identifier? (car exp)) (%free-identifier=? (car exp) 'lambda))
	(let* ((vars (%unwrap-syntax (cadr exp)))
	       (var (if (pair? vars) (car vars) vars))
	       (shift
		(if (and (syntactic-object? var)
			 (= (env-number env) (syntactic-number var)))
		    (- (syntactic-exponant var))
		    0)))
;	  (display "shift = ")(display shift) (newline)
	  (mk-closure
	   (unwrap-exp (%wrap-syntax (cadr exp) env shift))
	   (%wrap-syntax (cddr exp) env shift)
	   env)))

       ((and (%identifier? (car exp)) (%free-identifier=? (car exp) 'begin))
	(%evbody (cdr exp) env #f))
       
       ((and (%identifier? (car exp)) (%free-identifier=? (car exp) 'set!))
	(%set-value env
		    (cadr exp)
		    (%eval (caddr exp) env)))

       ((and (%identifier? (car exp)) (%free-identifier=? (car exp) 'define))
	(%set-value env
		    (cadr exp)
		    (%eval (caddr exp) env)))

       ((and (%identifier? (car exp)) (%free-identifier=? (car exp) 'make-transformer))
	(mk-transformer
	 (%eval (cadr exp) top-env)
	 env))

       ((and (%identifier? (car exp)) (%free-identifier=? (car exp) 'syntax))
	(let ((stx-env (slot-value (get-slot env '**syntax**))))
	  (if (eq? stx-env '**undefined**)
	      (%error '%eval "illegal use of syntax" exp)
	      (%syntax (cadr exp) (car stx-env) (cdr stx-env)))
	  ))

       (else
	(let ((fct (%eval (car exp) env)))
	  (if (%transformer? fct)

	      (let* ((trf     (transformer-fct fct))
		     (stx-env (transformer-env fct))
		     (cur-env env)
		     (form    (%wrap-syntax exp cur-env 1))

		     (x 
		      (if (< trace-expand trace-expand-level)
			  (begin (display "expand: ") (%display form) (newline))))
		     
		     (x (set! trace-expand (+ trace-expand 1)))

		     (expanded-form
		      (%evbody (unwrap-exp (closure-body trf))
			       (%add-env
				(%add-env (closure-env  trf)
					  (closure-vars trf)
					  (list form))
				(list '**syntax**)
				(list (cons cur-env stx-env)))
			       #f))

		     (x (set! trace-expand (- trace-expand 1))))

		(if (< trace-expand trace-expand-level)
		    (begin (display "expanded into: ") (%display expanded-form) (newline)))

		(%eval expanded-form env))

	      (%apply fct (%evlist (cdr exp) env))))
	))))
    
  (else exp)))

(define (%evlist exps env)
  (if (pair? exps)
      (cons (%eval   (car exps) env)
	    (%evlist (cdr exps) env))
      '()))

(define (%evbody body env last)
  (if (pair? body)
      (%evbody (cdr body) 
	       env
	       (%eval (car body) env))
      last))

(define (%apply fct args)
  (cond
   ((%subr?    fct) (apply (subr-body fct) args))
   ((%closure? fct)
    (%evbody (unwrap-exp (closure-body fct))
	     (%add-env (closure-env  fct)
		       (closure-vars fct)
		       args)
	     #f))
   (else 
    (%error '%apply "not a procedure" fct))
   ))

(define (%quote exp)
  (cond
   ((syntactic-object? exp) (%quote (syntactic-datum exp)))

   ((or (%subr?        exp)
	(%closure?     exp)
	(%transformer? exp))
    exp)

   ((pair? exp)
    (cons (%quote (car exp))
	  (%quote (cdr exp))))
   
   ((vector? exp)
    (list->vector (map %quote (vector->list exp))))

   (else exp)))

;;;
;;;  Syntactic objects
;;;

(define (mk-syntactic datum env expt)
  (vector 'syntactic datum env expt (env-number env)))

(define (syntactic-object? o)
  (and (vector? o) 
       (= 5 (vector-length o))
       (eq? 'syntactic (vector-ref o 0))))

(define (syntactic-datum  w) (vector-ref w 1))
(define (syntactic-env    w) (vector-ref w 2))
(define (syntactic-exponant   w) (vector-ref w 3))
(define (syntactic-number w) (vector-ref w 4))

;;;
;;;  Wrapping: identifiers must remain in the normal form
;;;   
;;;    + never have a null exponant
;;;    + exponant are always ordered
;;;

(define (%wrap-syntax datum env expt)
  (cond
   ((or (null?    datum)
	(number?  datum)
	(char?    datum)
	(boolean? datum))
    datum)

   ((not (syntactic-object? datum))
    (if (= 0 expt) datum (mk-syntactic datum env expt)))

   ((= (env-number env) (syntactic-number datum))

    (let ((expt (+ (syntactic-exponant datum) expt)))
      (if (= 0 expt)
	  (syntactic-datum datum)
	  (mk-syntactic (syntactic-datum datum) env expt))
      ))

   ((> (env-number env) (syntactic-number datum))
    (if (= 0 expt) datum (mk-syntactic datum env expt)))

   (else
    (%wrap-syntax (syntactic-datum datum) env expt))
   ))

;;;
;;;   SYNTAX special form
;;;

(define (%syntax exp cur-env stx-env)

  (define (syntax exp cur-env stx-env)
    (if (eq?  stx-env cur-env)
	(%wrap-syntax exp stx-env 1)
	(%wrap-syntax (syntax exp (cdr cur-env) stx-env) cur-env 1)))

  (%wrap-syntax (syntax exp cur-env stx-env) cur-env -1))

;;;
;;;   Low level facility (cf R4RS)
;;;
;;;   The only difference is that symbols are also identifiers
;;;

(define (%unwrap-syntax exp)

  (define (unwrap exp lst)
    (cond
     ((syntactic-object? exp)
      (unwrap (syntactic-datum exp) (cons exp lst)))

     ((or (%subr?        exp)
	  (%closure?     exp)
	  (%transformer? exp))
      exp)

     ((pair? exp)
      (cons (rewrap (car exp) lst)
	    (rewrap (cdr exp) lst)))
     
     ((vector? exp)
      (list->vector (map (lambda (x) 
			   (rewrap x lst))
			 (vector->list exp))))
     
     (else exp)
     ))

  (define (rewrap exp lst)
    (if (null? lst)
	exp
	(rewrap (%wrap-syntax exp
			      (syntactic-env      (car lst))
			      (syntactic-exponant (car lst)))
		(cdr lst))
	))
  
  (unwrap exp '()))

(define (%identifier? o)
  (cond
   ((symbol? o) #t)
   ((syntactic-object? o) (%identifier? (syntactic-datum o)))
   (else #f)))
  
(define (%bound-identifier=? id1 id2)
  (cond 

   ((and (symbol? id1) (symbol? id2)) (eq? id1 id2))

   ((not (syntactic-object? id1)) #f)
   ((not (syntactic-object? id2)) #f)

   ((and (= (syntactic-number   id1) (syntactic-number   id2))
	 (= (syntactic-exponant id1) (syntactic-exponant id2)))
    (%bound-identifier=? (syntactic-datum id1) (syntactic-datum id2)))

   (else #f)))
 
(define (%identifier->symbol id)
  (if (syntactic-object? id)
      (%identifier->symbol (syntactic-datum id))
      id))

(define (%free-identifier=? id1 id2)
  (let ((slot1 (if (syntactic-object? id1) 
		   (get-slot (syntactic-env id1) (syntactic-datum id1))
		   (get-slot top-env id1)))
	(slot2 (if (syntactic-object? id2) 
		   (get-slot (syntactic-env id2) (syntactic-datum id2))
		   (get-slot top-env id2))))
    (eq? slot1 slot2)))

(define %generate-identifier
  (let ((x -1))
    (lambda ()
      (set! x (+ x 1))
      (string->symbol
       (string-append "*G" (number->string x) "*"))
      )))

(define (%construct-identifier id symb)
  (if (syntactic-object? id)
      (%wrap-syntax (%construct-identifier (syntactic-datum id) symb)
		   (syntactic-env id)
		   (syntactic-exponant id))
      symb))

;;;  Usefull procedure ...

(define (unwrap-exp exp)
  (let ((exp (%unwrap-syntax exp)))
    (if (pair? exp)
	(cons (car exp) (unwrap-exp (cdr exp)))
	exp)))

;;;
;;;  Environments
;;;

(define (mk-slot var val num) (vector var val num))

(define (slot-id     s) (vector-ref s 0))
(define (slot-value  s) (vector-ref s 1))
(define (slot-number s) (vector-ref s 2))

(define (env-number  e) (slot-number (car e)))

(define (set-slot-value s v) (vector-set! s 1 v))

(define (get-slot env id)
  (if (not (pair? env))
      (let ((slot (mk-slot id '**undefined** 0)))
;	(display "new-slot: ") (%display id) (newline)
	(set-cdr! (last-pair top-env)
		  (cons slot '()))
	slot)

    (let* ((slot (car env)))
;      (display "slot ") (%display (slot-id slot)) (display " var ") (%display id) (display (%bound-identifier=? id (slot-id slot)))(newline)
      (cond

       ;;  Skip current slot if non zero exponant

       ((and (syntactic-object? id)
	     (not (= 0 (syntactic-exponant id)))
	     (eq? (syntactic-number id) (env-number (cdr env))))
	(get-slot (cdr env)
		  (syntactic-datum id)))

       ;;  As usual 

       ((%bound-identifier=? id (slot-id slot))	slot)

       ;;  Desyntaxify identifier if needed

       ((and (syntactic-object? id)
	     (eq? (syntactic-number id) (env-number (cdr env))))
	(get-slot (cdr env)
		  (syntactic-datum id)))

       ;;  Next slot

       (else (get-slot (cdr env) id))
       ))
    ))

(define (%add-env env vars vals)
  (cond 
   ((and (pair? vars) (pair? vals))
    (%add-env (cons (mk-slot (car vars)
			     (car vals)
			     (+ (env-number env) 1))
		    env)
	      (cdr vars)
	      (cdr vals)))

   ((not (null? vars))
    (cons (mk-slot vars vals (+ (env-number env) 1)) env))

   ((pair? vars) (%error '%add-env "missing values"  vars))
   ((pair? vals) (%error '%add-env "too many values" vals))

   (else env)))

(define (%value env var)
  (let ((val (slot-value (get-slot env var))))
    (if (eq? val '**undefined**)
	(%error '%eval "undefined symbol" var)
	val)))

(define (%set-value env var val)
  (set-slot-value (get-slot env var) val))

;;;
;;;   Usefull stuffs in order to have a full evaluator
;;;

(define (%procedure? o t) 
  (and (vector? o) 
       (>=  (vector-length o) 1)
       (eq? (vector-ref  o 0) t)))

(define (%subr?        o) (%procedure? o 'subr))
(define (%transformer? o) (%procedure? o 'transformer))
(define (%closure?     o) (%procedure? o 'closure))

(define (mk-transformer fct env)       (vector 'transformer fct  env))
(define (mk-closure     vars body env) (vector 'closure     vars body env))
(define (mk-subr        name fct)      (vector 'subr        fct  name))

(define (closure-vars     f) (vector-ref f 1))
(define (closure-body     f) (vector-ref f 2))
(define (closure-env      f) (vector-ref f 3))

(define (subr-body        s) (vector-ref s 1))
(define (subr-name        s) (vector-ref s 2))

(define (transformer-fct  t) (vector-ref t 1))
(define (transformer-env  t) (vector-ref t 2))

;;;  Printing ...

(define (%display obj)

  (define (display-list lst ch)
    (cond
     ((pair? lst)
      (display ch) 
      (%display (car lst))
      (display-list (cdr lst) #\space))
     ((null? lst) (display #\)))
     (else
      (display " . ")
      (%display lst)
      (display #\)))
     ))

  (define (display-wrapped exp)
    (%display   (syntactic-datum exp))
    (display    "@")
    (display    (env-number (syntactic-env exp)))
    (if (not (= 1 (syntactic-exponant exp)))
	(begin
	  (display    "^")
	  (display    (syntactic-exponant exp)))))

  (cond

   ((%subr?        obj)
    (display "#(primitive subr: ")
    (display (subr-name obj))
    (display #\)))

   ((%transformer? obj)
    (display "#(transformer: ")
    (%display (transformer-fct obj))
    (display #\)))

   ((%closure?     obj)
    (display "#(lambda ")
    (%display (closure-vars obj))
    (display-list (closure-body obj) " "))

   ((syntactic-object?      obj) (display-wrapped  obj))

   ((equal? '#()   obj) (display "#()"))

   ((pair?         obj) (display-list obj #\())
   ((vector?       obj) (display-list (vector->list obj) "#("))

   (else
    (display obj))
   ))

;;;  Main loop

(define (%map proc args)
  (if (memq '() args)
      '()
      (cons (%apply proc (map car args)) (%map proc (map cdr args)))))

(define (%load file)
  (with-input-from-file file
    (lambda ()
      (let loop ((obj (read)))
	(if (not (eof-object? obj))
	    (begin
	      (%eval obj top-env)
	      (loop (read))))
	))))

(define top-env
  (list
   (mk-slot 'symbol?	(mk-subr 'symbol? symbol?) 0)
   (mk-slot 'pair?	(mk-subr 'pair? pair?) 0)
   (mk-slot 'null?	(mk-subr 'null? null?) 0)
   (mk-slot 'list	(mk-subr 'list list) 0)
   (mk-slot 'cons	(mk-subr 'cons cons) 0)
   (mk-slot 'car	(mk-subr 'car car) 0)
   (mk-slot 'cdr	(mk-subr 'cdr cdr) 0)
   (mk-slot 'set-car!	(mk-subr 'set-car! set-car!) 0)
   (mk-slot 'set-cdr!	(mk-subr 'set-cdr! set-cdr!) 0)
   (mk-slot 'number?	(mk-subr 'number? number?) 0)
   (mk-slot '+		(mk-subr '+ +) 0)
   (mk-slot '*		(mk-subr '* *) 0)
   (mk-slot '-		(mk-subr '- -) 0)
   (mk-slot '=		(mk-subr '= =) 0)
   (mk-slot 'eq?	(mk-subr 'eq? eq?) 0)
   (mk-slot 'not	(mk-subr 'not not) 0)

   (mk-slot 'map	(mk-subr 'map (lambda (proc . args) (%map proc args))) 0)

   (mk-slot 'end	(mk-subr 'end (lambda () (set! %done #t))) 0)
   (mk-slot 'load	(mk-subr 'load %load) 0)
   
   (mk-slot 'identifier?		(mk-subr 'identifier?		%identifier?) 0)
   (mk-slot 'generate-identifier	(mk-subr 'generate-identifier	%generate-identifier) 0)
   (mk-slot 'construct-identifier	(mk-subr 'construct-identifier	%construct-identifier) 0)
   (mk-slot 'free-identifier=?		(mk-subr 'free-identifier=?	%free-identifier=?) 0)
   (mk-slot 'bound-identifier=?		(mk-subr 'bound-identifier=?	%bound-identifier=?) 0)
   (mk-slot 'identifier->symbol		(mk-subr 'identifier->symbol	%identifier->symbol) 0)
   (mk-slot 'unwrap-syntax		(mk-subr 'unwrap-syntax		%unwrap-syntax) 0)

   ))

(define %error #f)
(define %done  #f)

(define (main-loop)
  (set! %done #f)
  (call-with-current-continuation
   (lambda (k)

     (set! %error 
	   (lambda (fct msg obj)
	     (%display fct) (display ": ")
	     (%display msg) (display ": ")
	     (%display obj) (newline)
	     (k #f)))

     (display "?? ")

     (let ((obj (%eval (read) top-env)))
       (if (eof-object? obj)
	   (set! %done #t)
	   (begin
	     (display "== ") (%display obj) (newline))))
     ))

  (if (not %done)
      (main-loop)))

(main-loop)
