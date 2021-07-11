;-*-scheme-*-

(begin
(printf "chez-init.s Tue Nov 23 10:59:02 1993~%")

(define vi
  (lambda (filename)
    (system (string-append "vi " filename))
    (load filename)))

;; define-record

(define record-proc-names
  (lambda (name fields)
    (let ([name-str (symbol->string name)])
      (cons (string->symbol (string-append "make-" name-str))
	    (cons (string->symbol (string-append name-str "?"))
		  (map (lambda (field)
			 (string->symbol (string-append name-str "->"
					   (symbol->string field))))
		       fields))))))

(define record-indices
  (lambda (vec-len)
    (letrec ([loop (lambda (i)
		     (if (= i vec-len)
			 '()
			 (cons i (loop (+ i 1)))))])
      (loop 1))))

(if (not (top-level-bound? '**simples**))
  (define **simples** '()))

(extend-syntax (define-record)
  [(define-record name (field ...))
   (with ([(make-name name? name->field ...)
	   (record-proc-names 'name '(field ...))]
	  [record-length (+ 1 (length '(field ...)))])
     (with ([(i ...) (record-indices 'record-length)])
       (begin
	 (if (top-level-bound? '**simples**)
	   (set! **simples**
	     (append
	       (list 'make-name 'name? 'name->field ...)
	       **simples**)))
	 (define make-name
	   (lambda (field ...)
	     (list 'name field ...)))
	 (define name?
	   (lambda (obj)
	     (and (list? obj)
		  (= (length obj) record-length)
		  (eq? (list-ref obj 0) 'name))))
	 (define name->field
	   (lambda (obj)
	     (if (name? obj)
		 (list-ref obj i)
		 (error 'name->field "bad record ~s" obj))))
	 ...
	 'name)))])

(define make-record-from-name
  (lambda (name)
    (lambda args
      (cons name args))))

(printf "finished loading chez-init.s~%")
)
