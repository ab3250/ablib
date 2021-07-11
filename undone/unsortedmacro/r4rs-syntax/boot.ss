;;; boot.ss
;;; Robert Hieb & Kent Dybvig
;;; 90/10/07

; These macros are not fully correct;
; they are barely adequate for bootstrapping the system.
; "syntax" and "quasisyntax" always denote top-level syntax; 
; "quasisyntax" does not handle nested "quasi{quote,syntax}"
; expressions correctly.

; "boot-install-global-transformer" and "boot-make-global-syntax"
; must be exported by "expand.ss" during bootstrapping.

(extend-syntax (define-syntax)
   ((define-syntax x e)
    (boot-install-global-transformer 'x e)))

(extend-syntax (syntax)
   ((syntax x)
    (boot-make-global-syntax 'x)))

(extend-syntax (syntax-case else)
   ((syntax-case (x v))
    v)
   ((syntax-case (x v) (else e))
    (let ((x v)) e))
   ((syntax-case (x v) (p e) m ...)
    (syntax-dispatch v
                     (syntax p)
                     (lambda (x) e)
                     (lambda (x) (syntax-case (x x) m ...))))
   ((syntax-case (x v) (p f e) m ...)
    (let ((exp v)
          (fender (lambda (x) f))
          (body (lambda (x) e))
          (rest (lambda (x) (syntax-case (x x) m ...))))
       (syntax-dispatch exp
                        (syntax p)
                        (lambda (u) (if (fender u) (body u) (rest u)))
                        rest))))

(extend-syntax (quasisyntax unquote unquote-splicing)
   ((quasisyntax (unquote x))
    x)
   ((quasisyntax ((unquote-splicing x)))
    x)
   ((quasisyntax ((unquote-splicing x) . more))
    (append x (quasisyntax more)))
   ((quasisyntax (x . y))
    (cons (quasisyntax x) (quasisyntax y)))
   ((quasisyntax x)
    (vector? 'x)
    (with (((x ...) (vector->list 'x)))
       (list->vector (quasisyntax (x ...)))))
   ((quasisyntax x)
    (syntax x)))
  
