; The common semantic infrastructure.

; An environment maps names to denotations.

(define (binding env name)
  (env name))

(define (bind names denotations env)
  (let ((foo (map cons names denotations)))
    (lambda (name)
      (cond ((assoc name foo) => cdr)
	    (else (env name))))))

(define (bind-aliases uid mac env-of-use)
  (let ((env-of-definition (macro-env-of-definition mac)))
    (lambda (name)
      (if (and (generated? name)
	       (eqv? (generated-uid name) uid))
	  (env-of-definition (generated-name name))
	  (env-of-use name)))))

; A denotation is a location, a special operator, a macro, or unbound.

(define make-denotation cons)
(define denotation-kind car)
(define denotation-info cdr)

(define same-denotation? equal?)

(define (location? den) (eq? (denotation-kind den) 'location))
(define (special? den) (eq? (denotation-kind den) 'special))
(define (macro? den) (eq? (denotation-kind den) 'macro))

; Locations

(define (make-location arg)
  (make-denotation 'location (list arg)))

(define (location-place loc)
  (if (location? loc)
      (denotation-info loc)
      (error (if (or (special? loc) (macro? loc))
		 "keyword in invalid context"
		 "unbound variable")
	     loc)))

(define (value loc)		;Contents of a location
  (car (location-place loc)))

(define (set-value! loc value)	;Set contents of a location
  (set-car! (location-place loc) value))

; Special operators

(define (make-special name) ;Special operator denotation
  (make-denotation 'special name))

(define special-name denotation-info)

; Macros

(define (make-macro proc env-of-definition)
  (make-denotation 'macro (cons proc env-of-definition)))

(define (macro-transformer mac) (car (denotation-info mac)))
(define (macro-env-of-definition mac) (cdr (denotation-info mac)))

(define (transform mac exp env-of-use)
  (let* ((uid (generate-unique-id))
	 (env-for-expansion (bind-aliases uid mac env-of-use))
	 (rename (lambda (name)
		   (generate name uid)))
	 (compare
	  (lambda (name1 name2)
	    (or (eqv? name1 name2)
		(and (name? name1)
		     (name? name2)
		     (same-denotation? (binding env-for-expansion name1)
				       (binding env-for-expansion name2)))))))
    (values ((macro-transformer mac) exp rename compare)
	    uid)))

; Unbound

(define (make-unbound name)
  (make-denotation 'unbound name))

(define (make-random-denotation name)  ;hack for expander
  (make-denotation 'random (cons name (generate-unique-id))))


; Top level environment

(define top-level-env
  (lambda (name)
    (or (and (not (generated? name))
	     (table-ref top-level-table name))
	(make-unbound name))))

(define (define-top-level! name binding)
  (table-set! top-level-table name binding))

(define top-level-table (make-table))


; A name is either a symbol or a generated name.
; BIND assumes that generated names can be compared for identity using
; EQUAL?.  That's why we're representing them here as vectors.

(define (name? x)
  (or (symbol? x) (generated? x)))

(define (generate name uid) (vector name-marker name uid))

(define name-marker generate)

(define (generated? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (equal? (vector-ref x 0) name-marker)))

(define (generated-name name) (vector-ref name 1))
(define (generated-uid name) (vector-ref name 2))

(define *unique-id* 0)

(define (generate-unique-id)
  (let ((uid *unique-id*))
    (set! *unique-id* (+ *unique-id* 1))
    uid))

; For QUOTE

(define (desyntaxify datum)
  (cond ((generated? datum)
	 (generated-name datum))
	((pair? datum)
	 (let ((x (desyntaxify (car datum)))
		(y (desyntaxify (cdr datum))))
	    (if (and (eq? x (car datum))
		     (eq? y (cdr datum)))
		datum
		(cons x y))))
	((vector? datum)
	 (let ((new (make-vector (vector-length datum) #f)))
	   (let loop ((i 0) (same? #t))
	     (if (>= i (vector-length datum))
		 (if same? datum new)
		 (let ((x (desyntaxify (vector-ref datum i))))
		   (vector-set! new i x)
		   (loop (+ i 1)
			 (and same? (eq? x (vector-ref datum i)))))))))
	(else datum)))
	 
; For simplifying macro output

(define (contains-generated? thing uid)
  (cond ((pair? thing)
	 (or (contains-generated? (car thing) uid)
	     (contains-generated? (cdr thing) uid)))
	((vector? thing)
	 (and (generated? thing)
	      (eqv? (generated-uid thing) uid)))
	(else #f)))

; For DEFINE

(define (parse-define exp)
  (if (pair? (cadr exp))
      (values (caadr exp)
	      `(lambda ,(cdadr exp) ,@(cddr exp))) ;Not hygienic - fix somehow
      (values (cadr exp) (caddr exp))))

; For DEFINE-SYNTAX

(define (eval-transformer form env)
  (if (pair? form)
      (case (car form)
	((syntax-rules)
	 (eval (process-rules (cddr form)    ;list of rules
			      (cadr form)    ;list of subkeywords
			      (lambda (name) name) ;rename
			      eq?)
	       (interaction-environment)))
	((transformer)			;Cf. Clinger's Lisp Pointers article
	 (eval (cadr form) (interaction-environment)))
	;; ((naive) (eval-naive (cadr form) env))
	(else (error "unrecognized syntax syntax" form)))
      (error "unrecognized syntax syntax" form)))

; For usual macros

(define (define-usual-macros)
  (for-each-usual-macro
   (lambda (name proc)
     (define-top-level! name (make-macro (lambda (e r c)
					   (apply proc r c (cdr e)))
					 top-level-env)))))

(define (make-promise thunk) (delay (thunk))) ;for DELAY macro
(define (unspecific) (if #f 0))		;for COND
(define (unassigned) '*unassigned*)	;for LETREC

; Initialize

(define (initialize)
  (set! *unique-id* 0)
  (for-each (lambda (name)
	      (define-top-level! name (make-special name)))
	    '(quote lambda if begin set! define
		    define-syntax let-syntax letrec-syntax with-aliases))
  (define-usual-macros)
  (define-top-level! '+ (make-location +))
  (define-top-level! '- (make-location -))
  (define-top-level! '* (make-location *))
  (define-top-level! '/ (make-location /))
  (define-top-level! '< (make-location <))
  (define-top-level! '= (make-location =))
  (define-top-level! '> (make-location >))
  (define-top-level! 'cons (make-location cons))
  (define-top-level! 'pair? (make-location pair?))
  (define-top-level! 'car (make-location car))
  (define-top-level! 'cdr (make-location cdr))
  (define-top-level! 'null? (make-location null?))
  (define-top-level! 'list (make-location list))
  (define-top-level! 'append (make-location append))
  (define-top-level! 'map (make-location map))
  (define-top-level! 'eq? (make-location eq?))
  (define-top-level! 'equal? (make-location equal?))
  (define-top-level! 'eqv? (make-location eqv?))
  (define-top-level! 'memv (make-location memv))
  (define-top-level! 'list->vector (make-location list->vector))
  (define-top-level! 'unspecific (make-location unspecific))
  (define-top-level! 'unassigned (make-location unassigned))
  (define-top-level! 'make-promise (make-location make-promise))
  (define-top-level! 'cadr (make-location cadr))
  (define-top-level! 'cddr (make-location cddr))
  (define-top-level! 'caddr (make-location caddr)))

(initialize)
