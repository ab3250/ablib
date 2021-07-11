;;; hooks.ss
;;; Robert Hieb & Kent Dybvig
;;; 91/01/02

; these may have to be modified for the host system;
; the following work in Chez Scheme

(define eval-hook
   (lambda (x)
      (eval x)))

; in Chez Scheme this reports
; "Error in <symbol-who>: <string-why> <object-what>."

(define error-hook
   (lambda (symbol-who string-why object-what)
      (error symbol-who "~a ~s" string-why object-what)))

; "New symbols" need to be unique with respect to free identifiers
; within the scope of the binding expression that introduces them.  If
; it were impossible to generate unique symbols, one could output
; identifiers during expansion and feed the result directly into the
; compiler, or make another pass to perform hygienic alpha
; substitution.  Then new-symbol-hook need only return some unique
; object.

(define new-symbol-hook
   (lambda (string)
      (string->uninterned-symbol string)))
