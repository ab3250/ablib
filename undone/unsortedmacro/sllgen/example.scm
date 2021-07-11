;;; Interpreter for Chapter 5/Lecture 4

;;; original: Tue Oct 18 11:02:26 1988
;;; revised Fri Oct 23 10:30:16 1992
;;; revised to use sllgen Tue Sep 28 17:09:43 1993
::: changed eval-rands to eval-exp-list Mon Mar 20 16:16:44 1995

(printf "interp5.scm Mon Mar 20 16:16:50 1995~%")

;;; Assumes sllgen.scm is loaded

;;; **********************************************************

;;; Top-level interface

(define run
  (lambda (string)
    (eval-exp (scan&parse string) init-env)))

;;; ***********************************************************

;;; Lexical Specification

(define automaton-5
  '((proc if then else let set! in =)   ; keywords
    (start-state
      ((#\space #\tab #\newline) #f)
      ((alphabetic  #\* #\+ #\- #\/ #\! #\= #\:)
       (arbno (numeric alphabetic #\* #\+ #\- #\/ #\! #\= #\:))
       identifier)
      ((numeric)
       (arbno numeric)
       number)
      (#\( lparen)
      (#\) rparen)
      (#\^ end-marker)
      (#\% comment-state))
    (comment-state
        (#\newline #f)
        (any comment-state))))

;;; ****************************************************************

;;; Grammar

(define grammar-5
  '((expression
      (number)
      lit-exp)
    (expression
      (identifier)
      var-exp)
    (expression
      (set! identifier expression)
      assign-exp)
    (expression
      (if expression then expression else expression)
      if-exp)
    (expression
      (let (arbno declaration) in expression)
      let-exp)
    (expression
      (proc lparen (arbno identifier) rparen expression)
      proc-exp)
    (expression
      (lparen expression (arbno expression) rparen)
      app-exp)
  (declaration
    (identifier = expression)
    decl)))


(define scan&parse
  (sllgen:string->tree automaton-5 grammar-5))

;;; End of syntactic specification

;;; ****************************************************************

;;; Data Structure Definitions for Interpreter

;;; ****************************************************************

;;; Cells

(define make-cell
  (lambda (value)
    (cons '*cell value)))

(define deref-cell cdr)

(define set-cell! set-cdr!)		; danger!

;;; ****************************************************************

;; Finite functions:  ribcage (list of frames)

; empty-ribcage ==> nil
; (extend-ribcage names vals ff) ==> ((names . vals) . ff)

(define the-empty-ribcage '())

(define extend-ribcage
  (lambda (names vals ribcage)
    (if (= (length names) (length vals))
      (cons (cons names vals) ribcage)
      (error 'extend-ribcage
	"wrong number of values. names: ~s values: ~s"
	names values))))

(define apply-ribcage
  (lambda (ribcage z)
    (if (null? ribcage)
      (error 'apply-ribcage "identifier ~s not found" z)
      (let ((names (caar ribcage))(vals (cdar ribcage))(f (cdr ribcage)))
	(if (memq z names)
	  (letrec
	    ;; can assume z will be found in names
	    ([loop (lambda (names vals)
		     (if (eqv? z (car names)) (car vals)
		       (loop (cdr names) (cdr vals))))])
	    (loop names vals))
	  (apply-ribcage f z))))))

;;; ****************************************************************

;;; Building environments from ribcages:

(define the-empty-env the-empty-ribcage)

(define extend-env 
  (lambda (names values env)
    (extend-ribcage names (map make-cell values) env)))

(define apply-env apply-ribcage)


;;; *****************************************************************

;;; Declarations

(define-record decl (var exp))

;;; Closures and procedures

(define-record closure (formals body env))

;;; *****************************************************************
;;; *****************************************************************

;;; The Interpreter Proper

(define eval-exp
  (lambda (exp env)
    (record-case exp
      (lit-exp (constant) constant)
      (var-exp (id) (deref-cell (apply-env env id)))
      (assign-exp (ident rhs-exp)
	(set-cell!
	  (apply-env env ident)
	  (eval-exp rhs-exp env)))
      (proc-exp (formals body)
	(make-closure
	  formals
	  body env))
      (if-exp (test-exp true-exp false-exp)
	(if (true-value? (eval-exp test-exp env))
	  (eval-exp true-exp env)
	  (eval-exp false-exp env)))
      (let-exp (decls body)
	(let ((ids (map decl->var decls))
	      (exps  (map decl->exp decls)))
	  (let ((new-env
		  (extend-env ids
		    (eval-exp-list exps env)
		    env)))
	    (eval-exp body new-env))))
      (app-exp (rator rands)
	(let ((proc (eval-exp rator env))
	      (args (eval-exp-list rands env)))
	  (apply-proc proc args)))
      (else (error 'eval-exp
	      "Bad abstract syntax: ~s" exp)))))

(define eval-exp-list
  (lambda (rands env)
    (map (lambda (exp) (eval-exp exp env)) rands)))

(define apply-proc
  (lambda (proc args)
    (record-case proc
      (primitive-proc (primop)
	(apply-primop primop args))
      (closure (formals body env)
	(eval-exp body
	  (extend-env
	    formals args env)))
      (else (error 'apply-proc "Bad Procedure ~s" proc)))))


;;; *****************************************************************

;;; Primops

(define-record primitive-proc (primop))

(define apply-primop
  (lambda (primop args)
    (case primop
      ((+-op)  (+ (car args) (cadr args)))
      ((--op)  (- (car args) (cadr args)))
      ((*-op)  (* (car args) (cadr args)))
      ((+1-op) (+ (car args) 1))
      ((-1-op) (- (car args) 1))
      (else (error 'apply-primop "Unknown Primop: ~s" primop)))))

(define true-value?
  (lambda (v)
    (not (zero? v))))

;;; *****************************************************************

;;; The Initial Environment

(define build-init-env
  (lambda (pairs)
    (extend-env
      (map car pairs)
      (map make-primitive-proc
           (map cadr pairs))
      the-empty-env)))


(define init-pairs
  '((+ +-op)
    (- --op)
    (* *-op)
    (add1 +1-op)
    (sub1 -1-op)))

(define init-env (build-init-env init-pairs))

;;; ***************************************************************

;;; Tests

(define pgm1 "1")

(define pgm2 "(add1 x)")		; this one should end on a
					; domain error

(define pgm3 "let x = 3 y = 4 in (+ x y)")

(define pgm4 "let f = proc (x) (add1 x) in (f 4)")  

(define pgm5 "(proc (x) (add1 x) 4)")

(define pgm6 "let x = 3 
              in let y = set! x (add1 x)
                 in x")

; Chez Scheme Version 4.1b
; Copyright (c) 1991 Cadence Research Systems

; > (load "~/.chezrc")
; loading chez-init.s
; finished loading chez-init.s
; > (load "~wand/pub/plangs/sllgen.scm")
; > (load "interp5.scm")
; interp5.scm Tue Sep 28 17:09:49 1993
; > (run pgm1)
; 1
; > (run pgm2)

; Error in apply-ribcage: identifier x not found.
; Type (debug) to enter the debugger.
; > (run pgm3)
; 7
; > (run pgm4)
; 5
; > (run pgm6)
; 4
; > 
