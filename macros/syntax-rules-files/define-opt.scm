#lang r7rs
(import (scheme base)(scheme write))
; define-opt: A concise definition allowing optional arguments.
; Example:
;
; (define-opt (foo arg1 arg2 (optional arg3 (arg4 init4))) body)
;
; The form define-opt is designed to be as compatible with DSSSL's
; extended define as possible -- while avoiding the non-standard
; lexical token #!optional. On systems that do support DSSSL (e.g.,
; Gambit, Bigloo, Kawa) our define-opt expands into DSSSL's extended
; define, which is implemented efficiently on these systems.
;
; Here's the relevant part of the DSSSL specification, lifted
; from Gambit's online documentation:

;   define-formals = formal-argument-list | r4rs-define-formals
;   formal-argument-list = reqs opts rest keys
;   reqs = required-formal-argument*
;   required-formal-argument = variable
;   opts = #!optional optional-formal-argument* | empty
;   optional-formal-argument = variable | ( variable initializer )
;   rest = #!rest rest-formal-argument | empty
;   rest-formal-argument = variable
;   keys = #!key keyword-formal-argument* | empty
;   keyword-formal-argument = variable | ( variable initializer )
;   initializer = expression
;   r4rs-lambda-formals = ( variable* ) | ( variable+ . variable ) | variable
;   r4rs-define-formals = variable* | variable* . variable
;
;   1. Variables in required-formal-arguments are bound to successive actual
;      arguments starting with the first actual argument. It shall be an error
;      if there are fewer actual arguments than required-formal-arguments.
;   2. Next variables in optional-formal-arguments are bound to remaining
;      actual arguments. If there are fewer remaining actual arguments than
;      optional-formal-arguments, then the variables are bound to the result
;      of evaluating initializer, if one was specified, and otherwise to #f.
;      The initializer is evaluated in an environment in which all previous
;      formal arguments have been bound.
;   It shall be an error for a variable to appear more than once in a
;   formal-argument-list.
;   It is unspecified whether variables receive their value by binding or by
;   assignment.
;
; Our define-opt does not currently support rest and keys arguments.
; Also, instead of #optional optional-formal-argument ...
; we write (optional optional-formal-argument ...)
; 
; Our define-opt is similar to PLT Scheme's opt-lambda. However, 
; the syntax of define-opt guarantees that optional arguments are 
; really at the very end of the arg list.


; Chez does not support DSSSL extended defines and lambdas.
; Caveat: (define-opt name-bindings body) cannot expand into
; (let-syntax ((helper-macro ...)) (helper-macro name-bindings body))
; where helper-macro will generate the valid define.
; The mere appearance of (let-syntax ...) tells the Scheme system
; that whatever define will be generated, it is meant for the _internal_
; context. For example, the following code
;
; (define-syntax tdefine
;   (syntax-rules ()
;     ((tdefine _args . _bodies)
;       (letrec-syntax
; 	((helper
; 	   (syntax-rules ()
; 	     ((helper args bodies) (define args . bodies)))))
; 	(helper _args _bodies)))))
; (tdefine (foo x) (display "OK") (display x) (newline))
; (foo 42)
;
; runs OK on Petite Chez but gives an error "definition in expression context"
; on Scheme48 and SCM (and, consequently, the binding to foo does not occur).


(define-syntax define-opt
  (syntax-rules (optional)
    ((define-opt (name . bindings) . bodies)
      (define-opt "seek-optional" bindings () ((name . bindings) . bodies)))

    ((define-opt "seek-optional" ((optional . _opt-bindings))
       (reqd ...) ((name . _bindings) . _bodies))
      (define (name reqd ... . _rest)
	(letrec-syntax
	  ((handle-opts
	     (syntax-rules ()
	       ((_ rest bodies (var init))
		 (let ((var (if (null? rest) init
			      (if (null? (cdr rest)) (car rest)
				(error "extra rest" rest)))))
		   . bodies))
	       ((_ rest bodies var) (handle-opts rest bodies (var #f)))
	       ((_ rest bodies (var init) . other-vars)
		 (let ((var (if (null? rest) init (car rest)))
		       (new-rest (if (null? rest) '() (cdr rest))))
		   (handle-opts new-rest bodies . other-vars)))
	       ((_ rest bodies var . other-vars)
		 (handle-opts rest bodies (var #f) . other-vars))
	       ((_ rest bodies)		; no optional args, unlikely
		 (let ((_ (or (null? rest) (error "extra rest" rest))))
		   . bodies)))))
	  (handle-opts _rest _bodies . _opt-bindings))))

    ((define-opt "seek-optional" (x . rest) (reqd ...) form)
      (define-opt "seek-optional" rest (reqd ... x) form))

    ((define-opt "seek-optional" not-a-pair reqd form)
      (define . form))			; No optional found, regular define

    ((define-opt name body)		; Just the definition for 'name',
      (define name body))		; for compatibilibility with define
))

;;*******************************************************************************
 (define-opt (foo x (optional (y 3) (z 5))) (+ x y z))
(define-opt (bar (optional (x 1)))(+ x 1))
(display (foo 1))(newline)
(display (foo 1 2))(newline)
(display (foo 1 2 3 ))(newline)
(display (bar))(newline)
(display (bar 4))(newline)


;(cerr nl "Verifying define-opt..." nl)
;(let
;    ()
;  (let ()
;    (define-opt (foo x (optional (y 3) (z 5))) (+ x y z))
;    ;(assert (failed? (foo)))
;    (assert (= (foo 1) 9))
;    (assert (= (foo 1 2) 8))
;    (assert (= (foo 1 2 3) 6))
;    (cond-expand ((not (or gambit bigloo))
;                  (assert (failed? (foo 1 2 3 4)))) (else #f))
;    )
;  (let ((i 1))				; The default expression is
;    (define-opt (cnt (optional (x i)))	; re-evaluated every time
;      (set! x (max x i))
;      (set! i (+ 1 x))
;      x)
;    (assert (= 1 (cnt)))
;    (assert (= 2 (cnt)))
;    (assert (= 5 (cnt 5)))
;    (assert (= 6 (cnt 4)))
;    )
;  (let ()				; The optional list may be empty
;    (define-opt (f (optional)) 1)	; (per DSSSL spec)
;    (define-opt (g x (optional)) x)
;    (assert (equal? (f) 1))
;    (cond-expand ((not (or gambit bigloo))
;                  (assert (failed? (f 1)))) (else #f))
;    ;(assert (failed? (g)))
;    (assert (equal? (g 2) 2))
;    (cond-expand ((not (or gambit bigloo))
;                  (assert (failed? (g 1 2)))) (else #f))
;    )
;  (let ()				; define-opt acts as a regular define
;    (define-opt (f . rest) rest)
;    (define-opt g 1)
;    (define-opt (h x . rest) (list x  rest))
;    (assert (equal? (f) '()))
;    (assert (equal? (f 1) '(1)))
;    (assert (equal? (f 1 2) '(1 2)))
;    (assert (equal? g 1))
;    ;(assert (failed? (h)))
;    (assert (equal? (h 1) '(1 ())))
;    (assert (equal? (h 1 2) '(1 (2))))
;    (assert (equal? (h 1 2 3) '(1 (2 3))))
;    )
;  ; Tests from Gambit's manual (which actually pertain to DSSSL's
;  ; #!optional. We see how faithful our correspondence to DSSSL is
;  (let ()
;    (define-opt (f a (optional b)) (list a b))
;    (assert (equal? (f 1) '(1 #f)))
;    (assert (equal? (f 1 2) '(1 2)))
;    (cond-expand ((not (or gambit bigloo))
;                  (assert (failed? (f 1 2 3)))) (else #f))
;    )
;  (let ()
;    (define-opt (g a (optional (b a) c (d (list a b c)))) (list a b c d))
;    ;(assert (failed? (g)))
;    (assert (equal? (g 3) '(3 3 #f (3 3 #f))))
;    (assert (equal? (g 3 4) '(3 4 #f (3 4 #f))))
;    (assert (equal? (g 3 4 5) '(3 4 5 (3 4 5))))
;    (assert (equal? (g 3 4 5 6) '(3 4 5 6)))
;    (cond-expand ((not (or gambit bigloo))
;                  (assert (failed? (g 3 4 5 6 7)))) (else #f))
;    )
;  )
