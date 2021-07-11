;;; loadpp.ss
;;; Robert Hieb & Kent Dybvig
;;; 90/10/07
;;; modified 91/01/02

;;; Certain Chez Scheme macros produce calls to "void", which is
;;; a procedure of no arguments that is supposed to return some
;;; innocuous but "unspecified" value, possibly one that is not
;;; printed by the read-eval-print loop.
(define void
   (lambda ()
      #f))

;;; Chez Scheme's quasiquote expander produces calls to list*, so
;;; include the following definition whenever you load .pp files
;;; unless it is already defined.
(define list*
   (lambda (x . l)
      (let f ((x x) (l l))
         (if (null? l)
             x
             (cons x (f (car l) (cdr l)))))))

;;; new-eval may have to be adapted for use in Scheme systems other
;;; than Chez Scheme.  Also, the use of the second (eval) argument to
;;; "load" in some of the calls below will likely need to be changed.
(define new-eval
   (lambda (x)
      (eval (expand x))))

;;; Top-level-defines---top-level define expands into set! in Chez
;;; Scheme, so the preprocessed files contain set!s to these
;;; variables.
(define syntax-dispatch #f)
(define global-environment #f)
(define expand #f)
(define unwrap-syntax #f)
(define identifier? #f)
(define free-identifier=? #f)
(define bound-identifier=? #f)
(define identifier->symbol #f)
(define generate-identifier #f)
(define syntax-error #f)
(define boot-install-global-transformer #f)
(define boot-make-global-identifier #f)

;;; The first batch of loads brings up the macro system from a set
;;; of preprocessed files.  No macro calls remain in the preprocessed
;;; files, and the only the core forms lambda, quote, if, set!,
;;; constants, variable references, and applications remain.
(load "hooks.ss")
(load "syntax-dispatch.pp")
(load "init.pp")
(load "expand.pp")
(load "quasisyntax.pp")
(load "syntax-rules.pp")
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
