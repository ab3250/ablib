;;; load.ss
;;; Robert Hieb & Kent Dybvig
;;; 90/10/07
;;; modified 91/01/02

;;; new-eval may have to be adapted for use in Scheme systems other
;;; than Chez Scheme.  Also, the use of the second (eval) argument to
;;; "load" in some of the calls below will likely need to be changed.
(define new-eval
   (lambda (x)
      (eval (expand x))))

;;; The first batch of loads brings up the macro system on top of
;;; the existing macro system.  If there is no existing macro
;;; system (or it can't swallow our macro definitions), try using
;;; the file loadpp.ss.
(load "hooks.ss")
(load "boot.ss")
(load "syntax-dispatch.ss")
(load "init.ss")
(load "expand.ss")
(load "quasisyntax.ss")
(load "syntax-rules.ss")
(load "macro-defs.ss" new-eval)
(load "quasiquote.ss" new-eval)

;;; This second batch of loads bootstraps the macro system.  It's
;;; really not necessary, but it does provide a reasonable trial of
;;; your sanity.
(load "expand.ss" new-eval)
(load "quasisyntax.ss" new-eval)
(load "syntax-rules.ss" new-eval)
(load "macro-defs.ss" new-eval)
(load "quasiquote.ss" new-eval)
(load "syntax-dispatch.ss" new-eval)

;;; In Chez Scheme, this enters a new top-level, using new-eval as
;;; the evaluator.
(new-cafe new-eval)
