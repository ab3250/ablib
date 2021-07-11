;;; expand.ss
;;; Robert Hieb & Kent Dybvig
;;; 90/10/07
;;; modified 91/01/02

;;; notes:

; The original implementation has been modified to support
; capturing via "construct-identifier".

; areas in which this implementation is more lax than a strict
; interpretation of the revised report:
;   -- a "begin" at the begining of a body is allowed to contain both
;      definitions and expressions, as long as the definitions precede
;      the expressions; the "begin" is simply spliced into the body
;      (mostly because it was easier that way;
;       modification requires rewriting "chi-body" and its helpers)
;   -- similarly, a top-level "begin" is allowed to contain definitions
;      and expressions in any order; they are treated as if all the
;      items in the body were entered separately in the given order
;   -- non-readable objects (procedures, ports, eof, circular objects)
;      are allowed in quoted data
;      (they can be most easily intercepted in "build-data")
;   -- all identifiers that don't have macro definitions and are
;      not bound lexically are assumed to be global variables
;      (this can be changed by installing all implemented identifiers
;       in "global-environment" and making "global-lookup" do
;       something else when it gets an unbound identifier)

; Top-level definitions of macro-introduced identifiers are allowed
; as long as the identifiers are not "escapees" from some local binding.
; This may not be appropriate for implementations in which the
; model is that bindings are created by definitions, as opposed to
; one in which initial values are assigned by definitions.

; Identifiers and "wrapped" expressions are implemented as vectors.
; They should be implemented in terms of some sort of
; (implementation-dependent) opaque structures.

; The global environment must be global to avoid bootstrapping problems.
; (If it is local to "expand.ss" loading it wipes out all the macro defs.)
; It should be implemented as a table rather than as an "alist."
; It is initialized in "init.ss".


(let ()

;;; output

; The following output routines can be tailored to feed a specific
; system or compiler.
; They are set up here to feed directly into the evaluator of
; a working Scheme system.
; Output builders that use variables are given both the identifier
; and a unique symbol (but see note for "new-symbol-hook" above).
; This identifier is ignored here, but a compiler might be happier
; with the identifier, and in general might prefer more structured
; input.

(define build-application
   (lambda (fun-exp arg-exps)
      `(,fun-exp ,@arg-exps)))

(define build-conditional
   (lambda (test-exp then-exp else-exp)
      `(if ,test-exp ,then-exp ,else-exp)))

(define build-lexical-reference
   (lambda (id var)
      var))

(define build-lexical-assignment
   (lambda (id var exp)
      `(set! ,var ,exp)))

(define build-global-reference
   (lambda (id var)
      var))

(define build-global-assignment
   (lambda (id var exp)
      `(set! ,var ,exp)))

(define build-lambda
   (lambda (ids vars exp)
      `(lambda ,vars ,exp)))

(define build-data
   (lambda (exp)
      `(quote ,exp)))

; "build-syntax" gets S-expressions containing identifier structures.
; This won't work if the host system copies the identifier stuctures.
(define build-syntax
   (lambda (exp)
      `(quote ,exp)))

(define build-sequence
   (lambda (exps)
      (if (null? (cdr exps))
          (car exps)
          `(begin ,@exps))))

(define build-letrec
   (lambda (ids vars val-exps body-exp)
      `(letrec ,(map list vars val-exps) ,body-exp)))

(define build-global-definition
   (lambda (id var val)
      `(define ,var ,val)))
             

;;; reporting errors

(define call-error
   (lambda (symbol-who string-why object-what)
      (error-hook symbol-who string-why object-what)))

(define syntax-err
   (lambda (message object)
      (call-error 'expand message (strip object))))

(define scope-error
   (lambda (id)
      (syntax-err "invalid context for bound identifier"
                  id)))

(define keyword-error
   (lambda (id)
      (syntax-err "invalid context for keyword" id)))

(define core-error
   (lambda (x)
      (syntax-err "invalid syntax"  x)))


;;; environments

; <environment> ::= ((<name> . <binding>) ...)

; the global environment should be implementated as a table
; rather than as an alist

; the global environment functions do not distinguish between
; "defined" and "undefined" variables; all variables that do not have
; core, special or macro bindings are assumed to be "defined";
; defining a variable simply removes any macro bindings for the identifier

(define global-extend-macro-env
   (lambda (x binding)
      (let ((a (assq x (global-environment))))
         (if a
             (set-cdr! a binding)
             (global-environment
                (cons (cons x binding) (global-environment)))))))

(define global-extend-variable-env
   (lambda (x)
      (let ((a (assq x (global-environment))))
         (if a (set-cdr! a #f)))))

(define global-lookup-macro
   (lambda (x)
      (let ((a (assq x (global-environment))))
         (and a (cdr a)))))

(define null-env '())

(define extend-macro-env
   (lambda (vars vals r)
      (if (null? vars)
          r
          (cons (cons (car vars) (car vals))
                (extend-macro-env (cdr vars) (cdr vals) r)))))

(define lookup-macro
   (lambda (id r)
      (let ((b (id-binding id)))
         (let ((a (assq b r)))
            (cond
               (a (cdr a))
               ((eq? b (id-name id)) (global-lookup-macro b))
               (else #f))))))

(define extend-variable-env
   (lambda (vars r)
      (append vars r)))

(define lookup-variable
   (lambda (id r)
      (let ((b (id-binding id)))
         (cond
            ((memq b r) 'lexical)
            ((and (eq? b (id-name id)) (not (global-lookup-macro b))) 'global)
            (else #f)))))

(define install-core-transformer
   (lambda (x p)
      (global-extend-macro-env x (make-binding 'core p))))

(define install-macro-transformer
   (lambda (x p)
      (global-extend-macro-env x (make-binding 'macro p))))


;;; macro bindings

; <binding> ::= (core . <procedure>) |
;               (macro . <procedure>) |
;               (special . #f)

(define make-binding cons)

(define binding-type car)

(define binding-value cdr)


;;; wrapping environments

; <environment> ::= (rib (<identifier> . <identifier>) ...) |
;                   (join <environment> . <environment>) |
;                   (mark . <mark>) |
;                   (top)

(define env-type car)

(define make-join-env
   (lambda (r1 r2)
      (cond
         ((equal? r1 '(rib)) r2)
         ((equal? r2 '(rib)) r1)
         (else `(join ,r1 . ,r2)))))

(define env-left cadr)

(define env-right cddr)

(define make-mark-env
   (lambda (m)
      `(mark . ,m)))

(define env-mark cdr)

(define make-rib-env
   (lambda (old-ids new-ids)
      `(rib ,@(map cons old-ids new-ids))))

(define rib-env-ids cdr)

(define rib-id-lhs car)

(define rib-id-rhs cdr)

(define make-top-env
   (lambda ()
      '(top)))

(define lookup-rib-id
   (lambda (id rib-ids)
      (and (not (null? rib-ids))
           (let ((first (car rib-ids)))
              (if (bound-id=? (rib-id-lhs first) id)
                  (rib-id-rhs first)
                  (lookup-rib-id id (cdr rib-ids)))))))


;;; marks

; marks must be unique and comparable

; <mark> ::= (mark) | (anti mark)

(define new-mark (lambda () (list 'mark)))

(define mark=?
   (lambda (x y)
      (or (eq? x y)
          (and (antimark? x)
               (antimark? y)
               (eq? (antimark x) (antimark y))))))

(define antimark
   (lambda (mark)
      (if (antimark? mark)
          (cdr mark)
          (cons 'anti mark))))

(define antimark?
   (lambda (mark)
      (eq? (car mark) 'anti)))

(define same-marks?
   (lambda (x y)
      (if (null? x)
          (null? y)
          (and (not (null? y))
               (mark=? (car x) (car y))
               (same-marks? (cdr x) (cdr y))))))


;;; wrapped syntax

; <wrapped syntax> ::= #(syntax <expression> <environment>)

(define wrapped?
   (lambda (x)
      (and (vector? x)
           (= (vector-length x) 3)
           (eq? (vector-ref x 0) 'syntax))))

(define make-wrapped
   (lambda (x r)
      (vector 'syntax x r)))

(define wrapped-expression (lambda (x) (vector-ref x 1)))

(define wrapped-environment (lambda (x) (vector-ref x 2)))


;;; identifiers

; <identifier> ::= #(identifier <name> (<mark> ...) <binding>)

(define id?
   (lambda (x)
      (and (vector? x)
           (= (vector-length x) 5)
           (eq? (vector-ref x 0) 'identifier))))

(define make-id
   (lambda (name marks binding environment)
      (vector 'identifier name marks binding environment)))

(define id-name (lambda (x) (vector-ref x 1)))

(define id-marks (lambda (x) (vector-ref x 2)))

(define id-binding (lambda (x) (vector-ref x 3)))

(define id-environment (lambda (x) (vector-ref x 4)))

(define free-id=?
   (lambda (i j)
      (eq? (id-binding i) (id-binding j))))

(define bound-id=?
   (lambda (i j)
       (and (eq? (id-binding i) (id-binding j))
            (same-marks? (id-marks i) (id-marks j)))))

(define bound-id-member?
   (lambda (id l)
      (and (not (null? l))
           (or (bound-id=? id (car l))
               (bound-id-member? id (cdr l))))))

; "check-ids" returns #f if the id list is ok,
; otherwise it returns the result of calling syntax-err (if it returns).
; It is quadratic on the length of the id list;
; long id lists could be sorted first to make it more efficient.
(define check-ids
   (lambda (ids)
      (let check ((l ids))
         (cond
            ((null? l) #f)
            ((not (id? (car l)))
             (syntax-err "invalid identifier in identifier list" ids))
            ((bound-id-member? (car l) (cdr l))
             (syntax-err "duplicate identifier in identifier list" ids))
            (else (check (cdr l)))))))


;;; manipulating syntax (wrapped expressions and identifiers)

(define unwrap
   (lambda (x)
      (if (wrapped? x)
          (let ((e (wrapped-expression x)) (r (wrapped-environment x)))
             (cond
                ((pair? e)
                 (cons (wrap (car e) r) (wrap (cdr e) r)))
                ((vector? e)
                 (list->vector (map (lambda (x) (wrap x r))
                                    (vector->list e))))
                (else e)))
          x)))

(define wrap
   (lambda (x r)
      (cond
         ((id? x)
          (let ((x (wrap-id x r)))
             (make-id (id-name x)
                      (id-marks x)
                      (id-binding x)
                      (make-join-env r (id-environment x)))))
         ((wrapped? x)
          (make-wrapped (wrapped-expression x)
                        (make-join-env r (wrapped-environment x))))
         ((and (symbol? x) (contains-top-wrapping? r))
          (wrap-id (make-id x '() x r) r))
         (else (make-wrapped x r)))))

(define contains-top-wrapping?
   (lambda (r)
      (case (env-type r)
         ((join) (contains-top-wrapping? (env-right r)))
         ((top) #t)
         (else #f))))

(define wrap-id
   (lambda (id r)
      (case (env-type r)
         ((rib) (or (lookup-rib-id id (rib-env-ids r)) id))
         ((join) (wrap-id (wrap-id id (env-right r)) (env-left r)))
         ((mark) (add-mark (env-mark r) id))
         ((top) id))))

(define add-mark
   (lambda (m id)
      (make-id
         (id-name id)
         (let ((marks (id-marks id)))
            (cond
               ((null? marks)
                (list m))
               ((not (antimark? (car marks)))
                (cons m marks))
               ((and (not (antimark? m)) (mark=? (antimark (car marks)) m))
                (cdr marks))
               (else
                (syntax-err "invalid placement of identifier" id))))
         (id-binding id)
         (id-environment id))))

(define unwrap-list
   (lambda (x)
      (let ((x (unwrap x)))
         (if (pair? x)
             (cons (car x) (unwrap-list (cdr x)))
             x))))

(define make-wrapping-env
   (lambda (ids new-names)
      (make-rib-env ids
                    (map (lambda (id binding-name)
                            (make-id (id-name id)
                                     '()
                                     binding-name
                                     (id-environment id)))
                         ids
                         new-names))))


;;; expanding

(define chi
   (lambda (e mr vr)
      (let ((e (unwrap e)))
         (cond
            ((id? e)
             (chi-variable e mr vr))
            ((pair? e)
             (let ((first (car e)))
                (let ((b (and (id? first) (lookup-macro first mr))))
                   (if b
                       (case (binding-type b)
                          ((core) ((binding-value b) e mr vr))
                          ((macro) (chi (chi-macro-use (binding-value b) e)
                                        mr
                                        vr))
                          ((special) (chi-special e mr vr)))
                       (chi-application first (cdr e) mr vr)))))
            ((or (boolean? e) (number? e) (string? e) (char? e))
             (build-data e))
            (else (syntax-err "invalid program element" e))))))

(define chi-special
   (lambda (e mr vr)
      (syntax-case (e e)
         ((begin #t #t ...)
          (build-sequence (chi-list (cdr e) mr vr)))
         (else (syntax-err "invalid context for definition" e)))))

(define chi-top
   (lambda (e)
      (let ((e (unwrap e)))
         (let ((b (and (pair? e)
                       (id? (car e))
                       (lookup-macro (car e) null-env))))
            (cond
               ((and b (eq? (binding-type b) 'special))
                (syntax-case (e e)
                   ((define (#t . #t) #t #t ...)
                    (id? (caadr e))
                    (chi-global-define
                       (caadr e)
                       (quasisyntax (lambda ,(cdadr e) ,@(cddr e)))))
                   ((define #t #t)
                    (id? (cadr e))
                    (chi-global-define (cadr e) (caddr e)))
                   ((define #t)
                    (id? (cadr e))
                    (chi-global-define (cadr e) #f))
                   ((define-syntax #t #t)
                    (id? (cadr e))
                    (chi-global-define-syntax (cadr e) (caddr e)))
                   ((begin #t ...)
                    (let ((body (let dobody ((body (cdr e)))
                                   (if (null? body)
                                       '()
                                       (let ((first (chi-top (car body))))
                                          (cons first (dobody (cdr body))))))))
                       (if (null? body)
                           (build-data #f)
                           (build-sequence body))))
                   (else (core-error e))))
               ((and b (eq? (binding-type b) 'macro))
                (chi-top (chi-macro-use (binding-value b) e)))
               (else (chi e null-env null-env)))))))

(define chi-global-define
   (lambda (var val)
      (if (eq? (id-name var) (id-binding var))
          (begin
             (global-extend-variable-env var)
             (build-global-definition var
                                      (id-name var)
                                      (chi val null-env null-env)))
          (scope-error var))))

(define chi-global-define-syntax
   (lambda (var val)
      (if (eq? (id-name var) (id-binding var))
          (begin (global-extend-macro-env
                    (id-name var)
                    (chi-macro-def val null-env null-env))
                 (build-data #f))
          (scope-error var))))

(define chi-application
   (lambda (fun args mr vr)
      (build-application (chi fun mr vr) (chi-list (unwrap-list args) mr vr))))

(define chi-list
   (lambda (l mr vr)
      (map (lambda (x) (chi x mr vr)) l)))

(define chi-body
   (lambda (body mr vr e)
      (parse-body (unwrap-list body) '() '() '() '() mr vr e)))

(define parse-body
   (lambda (body var-ids var-vals macro-ids macro-vals mr vr e)
      (if (null? body)
          (syntax-err "no expressions in body of" e)
          (let ((next (unwrap (car body))))
             (let ((b (and (pair? next)
                           (id? (car next))
                           (lookup-macro (car next) mr))))
                (cond
                   ((and b (eq? (binding-type b) 'special))
                    (parse-special next
                                   (cdr body)
                                   var-ids
                                   var-vals
                                   macro-ids
                                   macro-vals
                                   mr
                                   vr
                                   e))
                   ((and b
                         (eq? (binding-type b) 'macro)
                         (not (bound-id-member? (car next) var-ids))
                         (not (bound-id-member? (car next) macro-ids)))
                    (parse-body (cons (chi-macro-use (binding-value b) next)
                                      (cdr body))
                                var-ids
                                var-vals
                                macro-ids
                                macro-vals
                                mr
                                vr
                                e))
                   (else (chi-build-body body
                                         (reverse var-ids)
                                         (reverse var-vals)
                                         (reverse macro-ids) 
                                         (reverse macro-vals)
                                         mr
                                         vr))))))))

(define parse-special
   (lambda (next rest var-ids var-vals macro-ids macro-vals mr vr e)
      (syntax-case (next next)
         ((define (#t . #t) #t #t ...)
          (id? (caadr next))
          (parse-body rest
                      (cons (caadr next) var-ids)
                      (cons (quasisyntax (lambda ,(cdadr next) ,@(cddr next)))
                             var-vals)
                      macro-ids
                      macro-vals
                      mr
                      vr
                      e))
         ((define #t #t)
          (id? (cadr next))
          (parse-body rest
                      (cons (cadr next) var-ids)
                      (cons (caddr next) var-vals)
                      macro-ids
                      macro-vals
                      mr
                      vr
                      e))
         ((define #t)
          (id? (cadr next))
          (parse-body rest
                      (cons (cadr next) var-ids)
                      (cons #f var-vals)
                      macro-ids
                      macro-vals
                      mr
                      vr
                      e))
         ((define-syntax #t #t)
          (id? (cadr next))
          (parse-body rest
                      var-ids
                      var-vals
                      (cons (cadr next) macro-ids)
                      (cons (caddr next) macro-vals)
                      mr
                      vr
                      e))
         ((begin #t ...)
          (parse-body (append (unwrap-list (cdr next)) rest)
                      var-ids
                      var-vals
                      macro-ids
                      macro-vals
                      mr
                      vr
                      e))
         (else (core-error next)))))

(define chi-build-body
   (lambda (body var-ids var-vals macro-ids macro-vals mr vr)
      (or (check-ids (append var-ids macro-ids))
          (let ((new-var-names (generate-formal-parameters var-ids))
                (new-macro-names (generate-formal-parameters macro-ids)))
             (let ((wr (make-wrapping-env
                          (append macro-ids var-ids)
                          (append new-macro-names new-var-names))))
                (let ((mr (extend-macro-env
                             new-macro-names
                             (map (lambda (x)
                                     (chi-macro-def (wrap x wr) mr vr))
                                  macro-vals)
                             mr))
                      (vr (extend-variable-env new-var-names vr)))
                   (let ((body (build-sequence
                                  (map (lambda (x)
                                          (chi (wrap x wr) mr vr))
                                       body))))
                      (if (null? var-ids)
                          body
                          (build-letrec var-ids
                                        new-var-names
                                        (map (lambda (x)
                                                (chi (wrap x wr) mr vr))
                                             var-vals)
                                        body)))))))))

(define chi-variable
   (lambda (id mr vr)
      (case (lookup-variable id vr)
         ((lexical) (build-lexical-reference id (id-binding id)))
         ((global) (build-global-reference id (id-name id)))
         (else
          (if (lookup-macro id mr)
              (keyword-error id)
              (scope-error id))))))

(define chi-assignment
   (lambda (id val mr vr)
      (case (lookup-variable id vr)
         ((lexical)
          (build-lexical-assignment id (id-binding id) (chi val mr vr)))
         ((global)
          (build-global-assignment id (id-name id) (chi val mr vr)))
         (else
          (if (lookup-macro id mr)
              (keyword-err id)
              (scope-error id))))))

(define chi-conditional
   (lambda (tst thn els mr vr)
      (build-conditional (chi tst mr vr)
                         (chi thn mr vr)
                         (chi els mr vr))))

(define chi-local-syntax
   (lambda (e recursive? mr vr)
      (syntax-case (e e)
         ((#t ((#t #t) ...) #t #t ...)
          (let ((vars (map car (cadr e)))
                      (vals (map cadr (cadr e)))
                      (body (cddr e)))
                   (or (check-ids vars)
                       (let ((new-vars (generate-formal-parameters vars)))
                          (let ((wr (make-wrapping-env vars new-vars)))
                             (chi-body (wrap body wr)
                                       (extend-macro-env
                                          new-vars
                                          (map (lambda (x)
                                                  (chi-macro-def (if recursive?
                                                                     (wrap x wr)
                                                                     x)
                                                                 mr
                                                                 vr))
                                               vals)
                                          mr)
                                       vr
                                       e))))))
         (else (core-error e)))))

(define chi-macro-def
   (lambda (x mr vr)
      (let ((p (eval-hook (chi x mr null-env))))
         (if (procedure? p)
             (make-binding 'macro p)
             (let ((err (syntax-err "non-procedural transformer" x)))
                (lambda (x) err))))))

(define chi-macro-use
   (lambda (transformer e)
      (let ((m (new-mark)))
         (wrap (transformer (wrap e (make-mark-env (antimark m))))
               (make-mark-env m)))))


;;; data

(define strip
   (lambda (x)
      (cond
         ((id? x) (id-name x))
         ((wrapped? x) (strip (wrapped-expression x)))
         ((pair? x) (cons (strip (car x)) (strip (cdr x))))
         ((vector? x) (list->vector (map strip (vector->list x))))
         (else x))))

(define pushdown-syntax
   (lambda (x)
      (cond
         ((id? x) x)
         ((wrapped? x) (pushdown-syntax (unwrap x)))
         ((pair? x) (cons (pushdown-syntax (car x)) (pushdown-syntax (cdr x))))
         ((vector? x) (list->vector (map pushdown-syntax (vector->list x))))
         (else x))))


;;; generating new lexical variables

(define generate-formal-parameters
   (lambda (params)
      (cond
         ((pair? params)
          (cons (make-new-variable (car params))
                (generate-formal-parameters (cdr params))))
         ((null? params) '())
         (else (make-new-variable params)))))

(define make-new-variable
   (lambda (id)
      (new-symbol-hook (symbol->string (id-name id)))))

(define flatten
   (lambda (x)
      (cond
         ((pair? x) (cons (car x) (flatten (cdr x))))
         ((null? x) '())
         (else (list x)))))


;;; special keywords

(global-extend-macro-env 'define (make-binding 'special #f))

(global-extend-macro-env 'define-syntax (make-binding 'special #f))

(global-extend-macro-env 'begin (make-binding 'special #f))


;;; core transformers

(install-core-transformer 'letrec-syntax
   (lambda (e mr vr)
      (chi-local-syntax e #t mr vr)))

(install-core-transformer 'let-syntax
   (lambda (e mr vr)
      (chi-local-syntax e #f mr vr)))

(install-core-transformer 'syntax
   (lambda (e mr vr)
      (syntax-case (e e)
         ((syntax #t)
          (build-syntax (pushdown-syntax (cadr e))))
         (else (core-error e)))))

(install-core-transformer 'quote
   (lambda (e mr vr)
      (syntax-case (e e)
         ((quote #t)
          (build-data (strip (cadr e))))
         (else (core-error e)))))

(install-core-transformer 'lambda
   (lambda (e mr vr)
      (syntax-case (e e)
         ((lambda #t #t #t ...)
          (let ((params (unwrap-list (cadr e)))
                (body (cddr e)))
             (let ((param-list (flatten params)))
                (or (check-ids param-list)
                    (let ((new-params (generate-formal-parameters params)))
                       (let ((new-param-list (flatten new-params)))
                          (build-lambda
                             params
                             new-params
                             (chi-body (wrap body
                                             (make-wrapping-env param-list
                                                                new-param-list))
                                       mr
                                       (extend-variable-env new-param-list vr)
                                       e))))))))
         (else (core-error e)))))

(install-core-transformer 'letrec
   (lambda (e mr vr)
      (syntax-case (e e)
         ((letrec ((#t #t) ...) #t #t ...)
          (let ((vars (map car (cadr e)))
                (vals (map cadr (cadr e)))
                (body (cddr e)))
             (or (check-ids vars)
                 (if (null? vars)
                     (chi-body body mr vr e)
                     (let ((new-vars (generate-formal-parameters vars)))
                        (let ((wr (make-wrapping-env vars new-vars))
                              (vr (extend-variable-env new-vars vr)))
                           (build-letrec vars
                                         new-vars
                                         (map (lambda (x)
                                                 (chi (wrap x wr) mr vr))
                                              vals)
                                         (chi-body (wrap body wr)
                                                   mr
                                                   vr
                                                   e))))))))
         (else (core-error e)))))

(install-core-transformer 'if
   (lambda (e mr vr)
      (syntax-case (e e)
         ((if #t #t)
          (chi-conditional (cadr e) (caddr e) #f mr vr))
         ((if #t #t #t)
          (chi-conditional (cadr e) (caddr e) (cadddr e) mr vr))
        (else (core-error e)))))

(install-core-transformer 'set!
   (lambda (e mr vr)
      (syntax-case (e e)
         ((set! #t #t)
          (id? (cadr e))
          (chi-assignment (cadr e) (caddr e) mr vr))
         (else (core-error e)))))


;;; exports

(set! expand
   (lambda (x)
      (chi-top (wrap x (make-top-env)))))

(set! generate-identifier
   (lambda ls 
      (cond
         [(null? ls)
          (generate-identifier (string->symbol "generated-identifier"))]
         [(null? (cdr ls))
          (let ([s (car ls)])
             (if (symbol? s)
                 (make-id s '() (new-symbol-hook (symbol->string s))
                    (make-top-env))
                 (call-error 'generate-identifier "non-symbol argument" s)))]
         [else (call-error 'generate-identifier "too many arguments" ls)])))

(set! construct-identifier
   (lambda (id sym)
      (cond
         ((not (identifier? id))
          (call-error 'construct-identifier "non-identifier argument" id))
         ((not (symbol? sym))
          (call-error 'construct-identifier "non-symbol argument" sym))
         (else
          (wrap sym (id-environment id))))))

(set! unwrap-syntax
   (lambda (x)
      (unwrap x)))

(set! free-identifier=?
   (lambda (x y)
      (if (and (id? x) (id? y))
          (free-id=? x y)
          (call-error 'free-identifier=?
                      "non-identifier argument"
                      (if (id? x) y x)))))

(set! bound-identifier=?
   (lambda (x y)
      (if (and (id? x) (id? y))
          (bound-id=? x y)
          (call-error 'bound-identifier=?
                      "non-identifier argument to bound-identifier=?"
                      (if (id? x) y x)))))

(set! identifier?
   (lambda (x)
      (id? x)))

(set! identifier->symbol
   (lambda (x)
      (unless (id? x)
         (call-error 'identifier->symbol "non-identifier argument" x))
      (id-name x)))

(set! syntax-error
   (lambda (message object)
      (if (string? message)
          (syntax-err message (strip object))
          (call-error 'syntax-error "non-string argument" message))))


;;; bootstrap exports

(set! boot-install-global-transformer
   (lambda (x e)
      (install-macro-transformer x e)))

(set! boot-make-global-syntax
   (lambda (x)
      (wrap x (make-top-env))))

)
