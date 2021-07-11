;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;;
;;;  define-record and record-case macros for MIT Scheme                   ;;;
;;;                                                                         ;;;
;;;     record-case is just a modifcation of the PC Scheme                 ;;;
;;;      and MacScheme macro by Jeff Alexander and Shinn-Der Lee.           ;;;
;;;                                                                         ;;;
;;;            Brent Benson (bwb@cs.unh.edu)                                ;;;
;;;                                                                         ;;;
;;;        Converted to positional representation for sllgen                ;;;
;;;                by Gary D. Duzan (duzan@ccs.neu.edu)                     ;;;
;;;                                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; The first two macros are so that the macro definition has
;;; desired affect both in code and at the REP loop.
;;;
(define-macro (define-macro-both pattern . body)
  `(begin
     (define-macro ,pattern ,@body)
     (syntax-table-define user-initial-syntax-table ',(car pattern)
       (macro ,(cdr pattern)
	 ,@body))))

(syntax-table-define user-initial-syntax-table 'define-macro-both
  (macro (pattern . body)
    `(begin
       (define-macro ,pattern ,@body)
       (syntax-table-define user-initial-syntax-table ',(car pattern)
	 (macro ,(cdr pattern)
	   ,@body)))))

;;;
;;; A couple of name differences
;;;
(define gensym generate-uninterned-symbol)
(define add1 1+)
(define (all-true? pred lst) (for-all? lst pred))
(define null-ended-list? list?)

(define error/define-record-or-record-case
  (lambda args
    (for-each (lambda (x) (display x) (display " ")) args)
    (newline)
    (error "Error from define-record or record-case.")))

(define-macro-both (define-record name fields)
  (if (and (symbol? name)
	   (list? fields))
      (let* ((name-str (symbol->string name))
	     (constructor (string->symbol (string-append "make-" name-str)))
	     (predicate (string->symbol (string-append name-str "?")))
	     (accessors (gen-accessors name-str fields
				       (lambda (x) (cdr x)))))
	`(begin
	   (define ,constructor (lambda args (cons ',name args)))
	   (define ,predicate (lambda (x) (and (list? x) (eq? (car x) ',name))))
	   ,@accessors))
      (error/define-record-or-record-case
	"define-record syntax error:" name)))

(define (gen-accessors name-str fields get-fields)
  (if (null? fields)
      '()
      (let* ((field-name (car fields))
	     (field-str (symbol->string field-name))
	     (acc-name 
               (string->symbol (string-append name-str "->" field-str))))
	(cons `(define ,acc-name (lambda (x) (car (,get-fields x))))
	      (gen-accessors name-str (cdr fields)
			     (lambda (x) (cdr (get-fields x))) )))))

(define-macro-both (record-case record-var . clauses)
  (let ((var (gensym)))
    (letrec
      ((loop
	 (lambda (clause)
	   (cond
             ((null? clause)
              `((#t (error/define-record-or-record-case
                                    "no clause matches:" ,var))))
             ((eq? (caar clause) 'else)
              (if (not (null? (cdr clause)))
                  (error/define-record-or-record-case
                    "record-case syntax error: clauses after an else."
                    (cdr clause))
                  `((#t ,@(cdar clause))) ))
             ((assoc (caar clause) (cdr clause))
              (error/define-record-or-record-case
                "record-case syntax error: duplicate clause:"
                (caar clause)))
             (else
	       (let ((name (symbol->string (caar clause))))
		 (cons
		   `((eq? (quote ,(caar clause)) (car ,var))
		     (let ,(let-vars name (cadar clause)
				     (lambda (x) (cdr x)) )
		       ,@(cddar clause)))
		   (loop (cdr clause)) ))))))
       (let-vars
	 (lambda (name fields get-fields)
	   (cond
	     ((null? fields) '())
	     ((member (car fields) (cdr fields))
	      (error/define-record-or-record-case
		"record-case syntax error: duplicate field. record:"
		(string-append name "," " field:") (car fields)))
	     (#t
               (cons
		 `(,(car fields)
		   (car (,get-fields ,var)))
		 (let-vars name (cdr fields)
		           (lambda (x) (cdr (get-fields x))) )))))))
      (if (and (all-true?
		 (lambda (clause)
		   (and (null-ended-list? clause)
			(not (null? clause))
			(symbol? (car clause))
			(if (eq? (car clause) 'else)
			    (not (null? (cdr clause)))
			    (and (> (length clause) 2)
				 (null-ended-list? (cadr clause))
				 (all-true? symbol? (cadr clause))))))
		 clauses))
	  `(let ((,var ,record-var))
	    (cond ,@(loop clauses)))
	  (error/define-record-or-record-case
	    "record-case syntax error:" record-var)) )))

(define make-record-from-name
  (lambda (name)
    (lambda args
      (cons name args))))

(define printf
  (lambda args
    (map display args)
    (newline) ))

#t


