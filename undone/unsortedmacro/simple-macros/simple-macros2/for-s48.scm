
; To load into Scheme 48:

;   ,load-config for-s48.scm
;   ,in top
;   y

(define-package ((top (export tst ex)))
  (open scheme signals evaluator expander syntax-system memoize)
  (files top))

(define-package ((evaluator (export ev set-memoizer!)))
  (open scheme syntax-system memoize signals memoize)
  (files ev))

(define-package ((expander (export expand set-clean?!)))
  (open scheme syntax-system signals)
  (files ex))

(define-package ((memoize (export memoize! memo-perhaps flush-memo-table!)))
  (open scheme table)
  (files memo))

(define-signature syntax-system-signature
  (export bind
	  bind-aliases
	  binding
	  contains-generated?
	  define-top-level!
	  desyntaxify
	  eval-transformer
	  generated-name
	  generated-uid
	  generated?
	  location?
	  macro?
	  make-macro
	  make-random-denotation
	  make-unbound
	  name?
	  parse-define
	  same-denotation?
	  special-name
	  special?
	  top-level-env
	  transform
	  value
	  set-value!
	  make-location))

(define-package ((syntax-system syntax-system-signature)
		 (name-predicate (export name?)))
  (open scheme signals table usual-macros pattern-language)
  (files usual rules syntax))


(define-package ((usual-macros (export for-each-usual-macro)))
  (open scheme signals)
  (files usual))

(define-package ((pattern-language (export process-rules)))
  (open scheme signals
	name-predicate)  ;Circular module dependency... uck.
  (files rules))


(define-package ((faster (export fa)))
  (open scheme syntax-system 
	evaluator expander
	enumerated signals
	primitives architecture ;for timing stuff
	)
  (files faster))