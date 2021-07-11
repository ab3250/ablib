((lambda ()
    ((lambda (null-env
              make-binding
              binding-type
              binding-value
              env-type
              env-left
              env-right
              env-mark
              rib-env-ids
              rib-id-lhs
              rib-id-rhs)
        ((lambda (build-application
                  build-conditional
                  build-lexical-reference
                  build-lexical-assignment
                  build-global-reference
                  build-global-assignment
                  build-lambda
                  build-data
                  build-syntax
                  build-sequence
                  build-letrec
                  build-global-definition
                  call-error
                  syntax-err
                  scope-error
                  keyword-error
                  core-error
                  global-extend-macro-env
                  global-extend-variable-env
                  global-lookup-macro
                  extend-macro-env
                  lookup-macro
                  extend-variable-env
                  lookup-variable
                  install-core-transformer
                  install-macro-transformer
                  make-join-env
                  make-mark-env
                  make-rib-env
                  make-top-env
                  lookup-rib-id
                  new-mark
                  mark=?
                  antimark
                  antimark?
                  same-marks?
                  wrapped?
                  make-wrapped
                  wrapped-expression
                  wrapped-environment
                  id?
                  make-id
                  id-name
                  id-marks
                  id-binding
                  id-environment
                  free-id=?
                  bound-id=?
                  bound-id-member?
                  check-ids
                  unwrap
                  wrap
                  contains-top-wrapping?
                  wrap-id
                  add-mark
                  unwrap-list
                  make-wrapping-env
                  chi
                  chi-special
                  chi-top
                  chi-global-define
                  chi-global-define-syntax
                  chi-application
                  chi-list
                  chi-body
                  parse-body
                  parse-special
                  chi-build-body
                  chi-variable
                  chi-assignment
                  chi-conditional
                  chi-local-syntax
                  chi-macro-def
                  chi-macro-use
                  strip
                  pushdown-syntax
                  generate-formal-parameters
                  make-new-variable
                  flatten)
            (set! build-application
               (lambda (fun-exp arg-exps) (list* fun-exp arg-exps)))
            (set! build-conditional
               (lambda (test-exp then-exp else-exp)
                  (list 'if test-exp then-exp else-exp)))
            (set! build-lexical-reference (lambda (id var) var))
            (set! build-lexical-assignment
               (lambda (id var exp) (list 'set! var exp)))
            (set! build-global-reference (lambda (id var) var))
            (set! build-global-assignment
               (lambda (id var exp) (list 'set! var exp)))
            (set! build-lambda
               (lambda (ids vars exp) (list 'lambda vars exp)))
            (set! build-data (lambda (exp) (list 'quote exp)))
            (set! build-syntax (lambda (exp) (list 'quote exp)))
            (set! build-sequence
               (lambda (exps)
                  (if (null? (cdr exps)) (car exps) (list* 'begin exps))))
            (set! build-letrec
               (lambda (ids vars val-exps body-exp)
                  (list 'letrec (map list vars val-exps) body-exp)))
            (set! build-global-definition
               (lambda (id var val) (list 'define var val)))
            (set! call-error
               (lambda (symbol-who string-why object-what)
                  (error-hook symbol-who string-why object-what)))
            (set! syntax-err
               (lambda (message object)
                  (call-error 'expand message (strip object))))
            (set! scope-error
               (lambda (id)
                  (syntax-err "invalid context for bound identifier" id)))
            (set! keyword-error
               (lambda (id) (syntax-err "invalid context for keyword" id)))
            (set! core-error
               (lambda (x) (syntax-err "invalid syntax" x)))
            (set! global-extend-macro-env
               (lambda (x binding)
                  ((lambda (a)
                      (if a
                          (set-cdr! a binding)
                          (global-environment
                             (cons (cons x binding)
                                   (global-environment)))))
                   (assq x (global-environment)))))
            (set! global-extend-variable-env
               (lambda (x)
                  ((lambda (a) (if a (set-cdr! a #f) (void)))
                   (assq x (global-environment)))))
            (set! global-lookup-macro
               (lambda (x)
                  ((lambda (a)
                      ((lambda (g0009) (if g0009 (cdr a) g0009)) a))
                   (assq x (global-environment)))))
            (set! extend-macro-env
               (lambda (vars vals r)
                  (if (null? vars)
                      r
                      (cons (cons (car vars) (car vals))
                            (extend-macro-env (cdr vars) (cdr vals) r)))))
            (set! lookup-macro
               (lambda (id r)
                  ((lambda (b)
                      ((lambda (a)
                          (if a
                              (cdr a)
                              (if (eq? b (id-name id))
                                  (global-lookup-macro b)
                                  #f)))
                       (assq b r)))
                   (id-binding id))))
            (set! extend-variable-env (lambda (vars r) (append vars r)))
            (set! lookup-variable
               (lambda (id r)
                  ((lambda (b)
                      (if (memq b r)
                          'lexical
                          (if ((lambda (g00010)
                                  (if g00010
                                      (not (global-lookup-macro b))
                                      g00010))
                               (eq? b (id-name id)))
                              'global
                              #f)))
                   (id-binding id))))
            (set! install-core-transformer
               (lambda (x p)
                  (global-extend-macro-env x (make-binding 'core p))))
            (set! install-macro-transformer
               (lambda (x p)
                  (global-extend-macro-env x (make-binding 'macro p))))
            (set! make-join-env
               (lambda (r1 r2)
                  (if (equal? r1 '(rib))
                      r2
                      (if (equal? r2 '(rib)) r1 (list* 'join r1 r2)))))
            (set! make-mark-env (lambda (m) (list* 'mark m)))
            (set! make-rib-env
               (lambda (old-ids new-ids)
                  (list* 'rib (map cons old-ids new-ids))))
            (set! make-top-env (lambda () '(top)))
            (set! lookup-rib-id
               (lambda (id rib-ids)
                  ((lambda (g00011)
                      (if g00011
                          ((lambda (first)
                              (if (bound-id=? (rib-id-lhs first) id)
                                  (rib-id-rhs first)
                                  (lookup-rib-id id (cdr rib-ids))))
                           (car rib-ids))
                          g00011))
                   (not (null? rib-ids)))))
            (set! new-mark (lambda () (list 'mark)))
            (set! mark=?
               (lambda (x y)
                  ((lambda (g00012)
                      (if g00012
                          g00012
                          ((lambda (g00013)
                              (if g00013
                                  ((lambda (g00014)
                                      (if g00014
                                          (eq? (antimark x) (antimark y))
                                          g00014))
                                   (antimark? y))
                                  g00013))
                           (antimark? x))))
                   (eq? x y))))
            (set! antimark
               (lambda (mark)
                  (if (antimark? mark) (cdr mark) (cons 'anti mark))))
            (set! antimark? (lambda (mark) (eq? (car mark) 'anti)))
            (set! same-marks?
               (lambda (x y)
                  (if (null? x)
                      (null? y)
                      ((lambda (g00015)
                          (if g00015
                              ((lambda (g00016)
                                  (if g00016
                                      (same-marks? (cdr x) (cdr y))
                                      g00016))
                               (mark=? (car x) (car y)))
                              g00015))
                       (not (null? y))))))
            (set! wrapped?
               (lambda (x)
                  ((lambda (g00017)
                      (if g00017
                          ((lambda (g00018)
                              (if g00018
                                  (eq? (vector-ref x 0) 'syntax)
                                  g00018))
                           (= (vector-length x) 3))
                          g00017))
                   (vector? x))))
            (set! make-wrapped (lambda (x r) (vector 'syntax x r)))
            (set! wrapped-expression (lambda (x) (vector-ref x 1)))
            (set! wrapped-environment (lambda (x) (vector-ref x 2)))
            (set! id?
               (lambda (x)
                  ((lambda (g00019)
                      (if g00019
                          ((lambda (g00020)
                              (if g00020
                                  (eq? (vector-ref x 0) 'identifier)
                                  g00020))
                           (= (vector-length x) 5))
                          g00019))
                   (vector? x))))
            (set! make-id
               (lambda (name marks binding environment)
                  (vector 'identifier name marks binding environment)))
            (set! id-name (lambda (x) (vector-ref x 1)))
            (set! id-marks (lambda (x) (vector-ref x 2)))
            (set! id-binding (lambda (x) (vector-ref x 3)))
            (set! id-environment (lambda (x) (vector-ref x 4)))
            (set! free-id=?
               (lambda (i j) (eq? (id-binding i) (id-binding j))))
            (set! bound-id=?
               (lambda (i j)
                  ((lambda (g00021)
                      (if g00021
                          (same-marks? (id-marks i) (id-marks j))
                          g00021))
                   (eq? (id-binding i) (id-binding j)))))
            (set! bound-id-member?
               (lambda (id l)
                  ((lambda (g00022)
                      (if g00022
                          ((lambda (g00023)
                              (if g00023
                                  g00023
                                  (bound-id-member? id (cdr l))))
                           (bound-id=? id (car l)))
                          g00022))
                   (not (null? l)))))
            (set! check-ids
               (lambda (ids)
                  (((lambda (check)
                       (set! check
                          (lambda (l)
                             (if (null? l)
                                 #f
                                 (if (not (id? (car l)))
                                     (syntax-err
                                        "invalid identifier in identifier list"
                                        ids)
                                     (if (bound-id-member? (car l) (cdr l))
                                         (syntax-err
                                            "duplicate identifier in identifier list"
                                            ids)
                                         (check (cdr l)))))))
                       check)
                    (void))
                   ids)))
            (set! unwrap
               (lambda (x)
                  (if (wrapped? x)
                      ((lambda (e r)
                          (if (pair? e)
                              (cons (wrap (car e) r) (wrap (cdr e) r))
                              (if (vector? e)
                                  (list->vector
                                     (map (lambda (x) (wrap x r))
                                          (vector->list e)))
                                  e)))
                       (wrapped-expression x)
                       (wrapped-environment x))
                      x)))
            (set! wrap
               (lambda (x r)
                  (if (id? x)
                      ((lambda (x)
                          (make-id
                             (id-name x)
                             (id-marks x)
                             (id-binding x)
                             (make-join-env r (id-environment x))))
                       (wrap-id x r))
                      (if (wrapped? x)
                          (make-wrapped
                             (wrapped-expression x)
                             (make-join-env r (wrapped-environment x)))
                          (if ((lambda (g00024)
                                  (if g00024
                                      (contains-top-wrapping? r)
                                      g00024))
                               (symbol? x))
                              (wrap-id (make-id x '() x r) r)
                              (make-wrapped x r))))))
            (set! contains-top-wrapping?
               (lambda (r)
                  ((lambda (g00025)
                      (if (eq? g00025 'join)
                          (contains-top-wrapping? (env-right r))
                          (if (eq? g00025 'top) #t #f)))
                   (env-type r))))
            (set! wrap-id
               (lambda (id r)
                  ((lambda (g00026)
                      (if (eq? g00026 'rib)
                          ((lambda (g00027) (if g00027 g00027 id))
                           (lookup-rib-id id (rib-env-ids r)))
                          (if (eq? g00026 'join)
                              (wrap-id
                                 (wrap-id id (env-right r))
                                 (env-left r))
                              (if (eq? g00026 'mark)
                                  (add-mark (env-mark r) id)
                                  (if (eq? g00026 'top) id (void))))))
                   (env-type r))))
            (set! add-mark
               (lambda (m id)
                  (make-id
                     (id-name id)
                     ((lambda (marks)
                         (if (null? marks)
                             (list m)
                             (if (not (antimark? (car marks)))
                                 (cons m marks)
                                 (if ((lambda (g00028)
                                         (if g00028
                                             (mark=?
                                                (antimark (car marks))
                                                m)
                                             g00028))
                                      (not (antimark? m)))
                                     (cdr marks)
                                     (syntax-err
                                        "invalid placement of identifier"
                                        id)))))
                      (id-marks id))
                     (id-binding id)
                     (id-environment id))))
            (set! unwrap-list
               (lambda (x)
                  ((lambda (x)
                      (if (pair? x)
                          (cons (car x) (unwrap-list (cdr x)))
                          x))
                   (unwrap x))))
            (set! make-wrapping-env
               (lambda (ids new-names)
                  (make-rib-env
                     ids
                     (map (lambda (id binding-name)
                             (make-id
                                (id-name id)
                                '()
                                binding-name
                                (id-environment id)))
                          ids
                          new-names))))
            (set! chi
               (lambda (e mr vr)
                  ((lambda (e)
                      (if (id? e)
                          (chi-variable e mr vr)
                          (if (pair? e)
                              ((lambda (first)
                                  ((lambda (b)
                                      (if b
                                          ((lambda (g00033)
                                              (if (eq? g00033 'core)
                                                  ((binding-value b)
                                                   e
                                                   mr
                                                   vr)
                                                  (if (eq? g00033 'macro)
                                                      (chi (chi-macro-use
                                                              (binding-value
                                                                 b)
                                                              e)
                                                           mr
                                                           vr)
                                                      (if (eq? g00033
                                                               'special)
                                                          (chi-special
                                                             e
                                                             mr
                                                             vr)
                                                          (void)))))
                                           (binding-type b))
                                          (chi-application
                                             first
                                             (cdr e)
                                             mr
                                             vr)))
                                   ((lambda (g00032)
                                       (if g00032
                                           (lookup-macro first mr)
                                           g00032))
                                    (id? first))))
                               (car e))
                              (if ((lambda (g00029)
                                      (if g00029
                                          g00029
                                          ((lambda (g00030)
                                              (if g00030
                                                  g00030
                                                  ((lambda (g00031)
                                                      (if g00031
                                                          g00031
                                                          (char? e)))
                                                   (string? e))))
                                           (number? e))))
                                   (boolean? e))
                                  (build-data e)
                                  (syntax-err
                                     "invalid program element"
                                     e)))))
                   (unwrap e))))
            (set! chi-special
               (lambda (e mr vr)
                  (syntax-dispatch
                     e
                     (boot-make-global-syntax '(begin #t #t ...))
                     (lambda (e) (build-sequence (chi-list (cdr e) mr vr)))
                     (lambda (e)
                        ((lambda (e)
                            (syntax-err
                               "invalid context for definition"
                               e))
                         e)))))
            (set! chi-top
               (lambda (e)
                  ((lambda (e)
                      ((lambda (b)
                          (if ((lambda (g00037)
                                  (if g00037
                                      (eq? (binding-type b) 'special)
                                      g00037))
                               b)
                              ((lambda (exp fender body rest)
                                  (syntax-dispatch
                                     exp
                                     (boot-make-global-syntax
                                        '(define (#t . #t) #t #t ...))
                                     (lambda (u)
                                        (if (fender u) (body u) (rest u)))
                                     rest))
                               e
                               (lambda (e) (id? (caadr e)))
                               (lambda (e)
                                  (chi-global-define
                                     (caadr e)
                                     (cons (boot-make-global-syntax
                                              'lambda)
                                           (cons (cdadr e) (cddr e)))))
                               (lambda (e)
                                  ((lambda (exp fender body rest)
                                      (syntax-dispatch
                                         exp
                                         (boot-make-global-syntax
                                            '(define #t #t))
                                         (lambda (u)
                                            (if (fender u)
                                                (body u)
                                                (rest u)))
                                         rest))
                                   e
                                   (lambda (e) (id? (cadr e)))
                                   (lambda (e)
                                      (chi-global-define
                                         (cadr e)
                                         (caddr e)))
                                   (lambda (e)
                                      ((lambda (exp fender body rest)
                                          (syntax-dispatch
                                             exp
                                             (boot-make-global-syntax
                                                '(define #t))
                                             (lambda (u)
                                                (if (fender u)
                                                    (body u)
                                                    (rest u)))
                                             rest))
                                       e
                                       (lambda (e) (id? (cadr e)))
                                       (lambda (e)
                                          (chi-global-define (cadr e) #f))
                                       (lambda (e)
                                          ((lambda (exp fender body rest)
                                              (syntax-dispatch
                                                 exp
                                                 (boot-make-global-syntax
                                                    '(define-syntax #t #t))
                                                 (lambda (u)
                                                    (if (fender u)
                                                        (body u)
                                                        (rest u)))
                                                 rest))
                                           e
                                           (lambda (e) (id? (cadr e)))
                                           (lambda (e)
                                              (chi-global-define-syntax
                                                 (cadr e)
                                                 (caddr e)))
                                           (lambda (e)
                                              (syntax-dispatch
                                                 e
                                                 (boot-make-global-syntax
                                                    '(begin #t ...))
                                                 (lambda (e)
                                                    ((lambda (body)
                                                        (if (null? body)
                                                            (build-data #f)
                                                            (build-sequence
                                                               body)))
                                                     (((lambda (dobody)
                                                          (set! dobody
                                                             (lambda (body)
                                                                (if (null?
                                                                       body)
                                                                    '()
                                                                    ((lambda (first)
                                                                        (cons first
                                                                              (dobody
                                                                                 (cdr body))))
                                                                     (chi-top
                                                                        (car body))))))
                                                          dobody)
                                                       (void))
                                                      (cdr e))))
                                                 (lambda (e)
                                                    ((lambda (e)
                                                        (core-error e))
                                                     e)))))))))))
                              (if ((lambda (g00036)
                                      (if g00036
                                          (eq? (binding-type b) 'macro)
                                          g00036))
                                   b)
                                  (chi-top
                                     (chi-macro-use (binding-value b) e))
                                  (chi e null-env null-env))))
                       ((lambda (g00034)
                           (if g00034
                               ((lambda (g00035)
                                   (if g00035
                                       (lookup-macro (car e) null-env)
                                       g00035))
                                (id? (car e)))
                               g00034))
                        (pair? e))))
                   (unwrap e))))
            (set! chi-global-define
               (lambda (var val)
                  (if (eq? (id-name var) (id-binding var))
                      ((lambda ()
                          (global-extend-variable-env var)
                          (build-global-definition
                             var
                             (id-name var)
                             (chi val null-env null-env))))
                      (scope-error var))))
            (set! chi-global-define-syntax
               (lambda (var val)
                  (if (eq? (id-name var) (id-binding var))
                      ((lambda ()
                          (global-extend-macro-env
                             (id-name var)
                             (chi-macro-def val null-env null-env))
                          (build-data #f)))
                      (scope-error var))))
            (set! chi-application
               (lambda (fun args mr vr)
                  (build-application
                     (chi fun mr vr)
                     (chi-list (unwrap-list args) mr vr))))
            (set! chi-list
               (lambda (l mr vr) (map (lambda (x) (chi x mr vr)) l)))
            (set! chi-body
               (lambda (body mr vr e)
                  (parse-body (unwrap-list body) '() '() '() '() mr vr e)))
            (set! parse-body
               (lambda (body var-ids var-vals macro-ids macro-vals mr vr e)
                  (if (null? body)
                      (syntax-err "no expressions in body of" e)
                      ((lambda (next)
                          ((lambda (b)
                              (if ((lambda (g00043)
                                      (if g00043
                                          (eq? (binding-type b) 'special)
                                          g00043))
                                   b)
                                  (parse-special
                                     next
                                     (cdr body)
                                     var-ids
                                     var-vals
                                     macro-ids
                                     macro-vals
                                     mr
                                     vr
                                     e)
                                  (if ((lambda (g00040)
                                          (if g00040
                                              ((lambda (g00041)
                                                  (if g00041
                                                      ((lambda (g00042)
                                                          (if g00042
                                                              (not (bound-id-member?
                                                                      (car next)
                                                                      macro-ids))
                                                              g00042))
                                                       (not (bound-id-member?
                                                               (car next)
                                                               var-ids)))
                                                      g00041))
                                               (eq? (binding-type b)
                                                    'macro))
                                              g00040))
                                       b)
                                      (parse-body
                                         (cons (chi-macro-use
                                                  (binding-value b)
                                                  next)
                                               (cdr body))
                                         var-ids
                                         var-vals
                                         macro-ids
                                         macro-vals
                                         mr
                                         vr
                                         e)
                                      (chi-build-body
                                         body
                                         (reverse var-ids)
                                         (reverse var-vals)
                                         (reverse macro-ids)
                                         (reverse macro-vals)
                                         mr
                                         vr))))
                           ((lambda (g00038)
                               (if g00038
                                   ((lambda (g00039)
                                       (if g00039
                                           (lookup-macro (car next) mr)
                                           g00039))
                                    (id? (car next)))
                                   g00038))
                            (pair? next))))
                       (unwrap (car body))))))
            (set! parse-special
               (lambda (next
                        rest
                        var-ids
                        var-vals
                        macro-ids
                        macro-vals
                        mr
                        vr
                        e)
                  ((lambda (exp fender body rest)
                      (syntax-dispatch
                         exp
                         (boot-make-global-syntax
                            '(define (#t . #t) #t #t ...))
                         (lambda (u) (if (fender u) (body u) (rest u)))
                         rest))
                   next
                   (lambda (next) (id? (caadr next)))
                   (lambda (next)
                      (parse-body
                         rest
                         (cons (caadr next) var-ids)
                         (cons (cons (boot-make-global-syntax 'lambda)
                                     (cons (cdadr next) (cddr next)))
                               var-vals)
                         macro-ids
                         macro-vals
                         mr
                         vr
                         e))
                   (lambda (next)
                      ((lambda (exp fender body rest)
                          (syntax-dispatch
                             exp
                             (boot-make-global-syntax '(define #t #t))
                             (lambda (u) (if (fender u) (body u) (rest u)))
                             rest))
                       next
                       (lambda (next) (id? (cadr next)))
                       (lambda (next)
                          (parse-body
                             rest
                             (cons (cadr next) var-ids)
                             (cons (caddr next) var-vals)
                             macro-ids
                             macro-vals
                             mr
                             vr
                             e))
                       (lambda (next)
                          ((lambda (exp fender body rest)
                              (syntax-dispatch
                                 exp
                                 (boot-make-global-syntax '(define #t))
                                 (lambda (u)
                                    (if (fender u) (body u) (rest u)))
                                 rest))
                           next
                           (lambda (next) (id? (cadr next)))
                           (lambda (next)
                              (parse-body
                                 rest
                                 (cons (cadr next) var-ids)
                                 (cons #f var-vals)
                                 macro-ids
                                 macro-vals
                                 mr
                                 vr
                                 e))
                           (lambda (next)
                              ((lambda (exp fender body rest)
                                  (syntax-dispatch
                                     exp
                                     (boot-make-global-syntax
                                        '(define-syntax #t #t))
                                     (lambda (u)
                                        (if (fender u) (body u) (rest u)))
                                     rest))
                               next
                               (lambda (next) (id? (cadr next)))
                               (lambda (next)
                                  (parse-body
                                     rest
                                     var-ids
                                     var-vals
                                     (cons (cadr next) macro-ids)
                                     (cons (caddr next) macro-vals)
                                     mr
                                     vr
                                     e))
                               (lambda (next)
                                  (syntax-dispatch
                                     next
                                     (boot-make-global-syntax
                                        '(begin #t ...))
                                     (lambda (next)
                                        (parse-body
                                           (append
                                              (unwrap-list (cdr next))
                                              rest)
                                           var-ids
                                           var-vals
                                           macro-ids
                                           macro-vals
                                           mr
                                           vr
                                           e))
                                     (lambda (next)
                                        ((lambda (next) (core-error next))
                                         next)))))))))))))
            (set! chi-build-body
               (lambda (body var-ids var-vals macro-ids macro-vals mr vr)
                  ((lambda (g00044)
                      (if g00044
                          g00044
                          ((lambda (new-var-names new-macro-names)
                              ((lambda (wr)
                                  ((lambda (mr vr)
                                      ((lambda (body)
                                          (if (null? var-ids)
                                              body
                                              (build-letrec
                                                 var-ids
                                                 new-var-names
                                                 (map (lambda (x)
                                                         (chi (wrap x wr)
                                                              mr
                                                              vr))
                                                      var-vals)
                                                 body)))
                                       (build-sequence
                                          (map (lambda (x)
                                                  (chi (wrap x wr) mr vr))
                                               body))))
                                   (extend-macro-env
                                      new-macro-names
                                      (map (lambda (x)
                                              (chi-macro-def
                                                 (wrap x wr)
                                                 mr
                                                 vr))
                                           macro-vals)
                                      mr)
                                   (extend-variable-env new-var-names vr)))
                               (make-wrapping-env
                                  (append macro-ids var-ids)
                                  (append new-macro-names new-var-names))))
                           (generate-formal-parameters var-ids)
                           (generate-formal-parameters macro-ids))))
                   (check-ids (append var-ids macro-ids)))))
            (set! chi-variable
               (lambda (id mr vr)
                  ((lambda (g00045)
                      (if (eq? g00045 'lexical)
                          (build-lexical-reference id (id-binding id))
                          (if (eq? g00045 'global)
                              (build-global-reference id (id-name id))
                              (if (lookup-macro id mr)
                                  (keyword-error id)
                                  (scope-error id)))))
                   (lookup-variable id vr))))
            (set! chi-assignment
               (lambda (id val mr vr)
                  ((lambda (g00046)
                      (if (eq? g00046 'lexical)
                          (build-lexical-assignment
                             id
                             (id-binding id)
                             (chi val mr vr))
                          (if (eq? g00046 'global)
                              (build-global-assignment
                                 id
                                 (id-name id)
                                 (chi val mr vr))
                              (if (lookup-macro id mr)
                                  (keyword-err id)
                                  (scope-error id)))))
                   (lookup-variable id vr))))
            (set! chi-conditional
               (lambda (tst thn els mr vr)
                  (build-conditional
                     (chi tst mr vr)
                     (chi thn mr vr)
                     (chi els mr vr))))
            (set! chi-local-syntax
               (lambda (e recursive? mr vr)
                  (syntax-dispatch
                     e
                     (boot-make-global-syntax
                        '(#t ((#t #t) ...) #t #t ...))
                     (lambda (e)
                        ((lambda (vars vals body)
                            ((lambda (g00047)
                                (if g00047
                                    g00047
                                    ((lambda (new-vars)
                                        ((lambda (wr)
                                            (chi-body
                                               (wrap body wr)
                                               (extend-macro-env
                                                  new-vars
                                                  (map (lambda (x)
                                                          (chi-macro-def
                                                             (if recursive?
                                                                 (wrap x
                                                                       wr)
                                                                 x)
                                                             mr
                                                             vr))
                                                       vals)
                                                  mr)
                                               vr
                                               e))
                                         (make-wrapping-env
                                            vars
                                            new-vars)))
                                     (generate-formal-parameters vars))))
                             (check-ids vars)))
                         (map car (cadr e))
                         (map cadr (cadr e))
                         (cddr e)))
                     (lambda (e) ((lambda (e) (core-error e)) e)))))
            (set! chi-macro-def
               (lambda (x mr vr)
                  ((lambda (p)
                      (if (procedure? p)
                          (make-binding 'macro p)
                          ((lambda (err) (lambda (x) err))
                           (syntax-err "non-procedural transformer" x))))
                   (eval-hook (chi x mr null-env)))))
            (set! chi-macro-use
               (lambda (transformer e)
                  ((lambda (m)
                      (wrap (transformer
                               (wrap e (make-mark-env (antimark m))))
                            (make-mark-env m)))
                   (new-mark))))
            (set! strip
               (lambda (x)
                  (if (id? x)
                      (id-name x)
                      (if (wrapped? x)
                          (strip (wrapped-expression x))
                          (if (pair? x)
                              (cons (strip (car x)) (strip (cdr x)))
                              (if (vector? x)
                                  (list->vector
                                     (map strip (vector->list x)))
                                  x))))))
            (set! pushdown-syntax
               (lambda (x)
                  (if (id? x)
                      x
                      (if (wrapped? x)
                          (pushdown-syntax (unwrap x))
                          (if (pair? x)
                              (cons (pushdown-syntax (car x))
                                    (pushdown-syntax (cdr x)))
                              (if (vector? x)
                                  (list->vector
                                     (map pushdown-syntax
                                          (vector->list x)))
                                  x))))))
            (set! generate-formal-parameters
               (lambda (params)
                  (if (pair? params)
                      (cons (make-new-variable (car params))
                            (generate-formal-parameters (cdr params)))
                      (if (null? params) '() (make-new-variable params)))))
            (set! make-new-variable
               (lambda (id)
                  (new-symbol-hook (symbol->string (id-name id)))))
            (set! flatten
               (lambda (x)
                  (if (pair? x)
                      (cons (car x) (flatten (cdr x)))
                      (if (null? x) '() (list x)))))
            (global-extend-macro-env 'define (make-binding 'special #f))
            (global-extend-macro-env
               'define-syntax
               (make-binding 'special #f))
            (global-extend-macro-env 'begin (make-binding 'special #f))
            (install-core-transformer
               'letrec-syntax
               (lambda (e mr vr) (chi-local-syntax e #t mr vr)))
            (install-core-transformer
               'let-syntax
               (lambda (e mr vr) (chi-local-syntax e #f mr vr)))
            (install-core-transformer
               'syntax
               (lambda (e mr vr)
                  (syntax-dispatch
                     e
                     (boot-make-global-syntax '(syntax #t))
                     (lambda (e) (build-syntax (pushdown-syntax (cadr e))))
                     (lambda (e) ((lambda (e) (core-error e)) e)))))
            (install-core-transformer
               'quote
               (lambda (e mr vr)
                  (syntax-dispatch
                     e
                     (boot-make-global-syntax ''#t)
                     (lambda (e) (build-data (strip (cadr e))))
                     (lambda (e) ((lambda (e) (core-error e)) e)))))
            (install-core-transformer
               'lambda
               (lambda (e mr vr)
                  (syntax-dispatch
                     e
                     (boot-make-global-syntax '(lambda #t #t #t ...))
                     (lambda (e)
                        ((lambda (params body)
                            ((lambda (param-list)
                                ((lambda (g0008)
                                    (if g0008
                                        g0008
                                        ((lambda (new-params)
                                            ((lambda (new-param-list)
                                                (build-lambda
                                                   params
                                                   new-params
                                                   (chi-body
                                                      (wrap body
                                                            (make-wrapping-env
                                                               param-list
                                                               new-param-list))
                                                      mr
                                                      (extend-variable-env
                                                         new-param-list
                                                         vr)
                                                      e)))
                                             (flatten new-params)))
                                         (generate-formal-parameters
                                            params))))
                                 (check-ids param-list)))
                             (flatten params)))
                         (unwrap-list (cadr e))
                         (cddr e)))
                     (lambda (e) ((lambda (e) (core-error e)) e)))))
            (install-core-transformer
               'letrec
               (lambda (e mr vr)
                  (syntax-dispatch
                     e
                     (boot-make-global-syntax
                        '(letrec ((#t #t) ...) #t #t ...))
                     (lambda (e)
                        ((lambda (vars vals body)
                            ((lambda (g0007)
                                (if g0007
                                    g0007
                                    (if (null? vars)
                                        (chi-body body mr vr e)
                                        ((lambda (new-vars)
                                            ((lambda (wr vr)
                                                (build-letrec
                                                   vars
                                                   new-vars
                                                   (map (lambda (x)
                                                           (chi (wrap x wr)
                                                                mr
                                                                vr))
                                                        vals)
                                                   (chi-body
                                                      (wrap body wr)
                                                      mr
                                                      vr
                                                      e)))
                                             (make-wrapping-env
                                                vars
                                                new-vars)
                                             (extend-variable-env
                                                new-vars
                                                vr)))
                                         (generate-formal-parameters
                                            vars)))))
                             (check-ids vars)))
                         (map car (cadr e))
                         (map cadr (cadr e))
                         (cddr e)))
                     (lambda (e) ((lambda (e) (core-error e)) e)))))
            (install-core-transformer
               'if
               (lambda (e mr vr)
                  (syntax-dispatch
                     e
                     (boot-make-global-syntax '(if #t #t))
                     (lambda (e)
                        (chi-conditional (cadr e) (caddr e) #f mr vr))
                     (lambda (e)
                        (syntax-dispatch
                           e
                           (boot-make-global-syntax '(if #t #t #t))
                           (lambda (e)
                              (chi-conditional
                                 (cadr e)
                                 (caddr e)
                                 (cadddr e)
                                 mr
                                 vr))
                           (lambda (e)
                              ((lambda (e) (core-error e)) e)))))))
            (install-core-transformer
               'set!
               (lambda (e mr vr)
                  ((lambda (exp fender body rest)
                      (syntax-dispatch
                         exp
                         (boot-make-global-syntax '(set! #t #t))
                         (lambda (u) (if (fender u) (body u) (rest u)))
                         rest))
                   e
                   (lambda (e) (id? (cadr e)))
                   (lambda (e) (chi-assignment (cadr e) (caddr e) mr vr))
                   (lambda (e) ((lambda (e) (core-error e)) e)))))
            (set! expand (lambda (x) (chi-top (wrap x (make-top-env)))))
            (set! generate-identifier
               (lambda ls
                  (if (null? ls)
                      (generate-identifier
                         (string->symbol "generated-identifier"))
                      (if (null? (cdr ls))
                          ((lambda (s)
                              (if (symbol? s)
                                  (make-id
                                     s
                                     '()
                                     (new-symbol-hook (symbol->string s))
                                     (make-top-env))
                                  (call-error
                                     'generate-identifier
                                     "non-symbol argument"
                                     s)))
                           (car ls))
                          (call-error
                             'generate-identifier
                             "too many arguments"
                             ls)))))
            (set! construct-identifier
               (lambda (id sym)
                  (if (not (identifier? id))
                      (call-error
                         'construct-identifier
                         "non-identifier argument"
                         id)
                      (if (not (symbol? sym))
                          (call-error
                             'construct-identifier
                             "non-symbol argument"
                             sym)
                          (wrap sym (id-environment id))))))
            (set! unwrap-syntax (lambda (x) (unwrap x)))
            (set! free-identifier=?
               (lambda (x y)
                  (if ((lambda (g0006) (if g0006 (id? y) g0006)) (id? x))
                      (free-id=? x y)
                      (call-error
                         'free-identifier=?
                         "non-identifier argument"
                         (if (id? x) y x)))))
            (set! bound-identifier=?
               (lambda (x y)
                  (if ((lambda (g0005) (if g0005 (id? y) g0005)) (id? x))
                      (bound-id=? x y)
                      (call-error
                         'bound-identifier=?
                         "non-identifier argument to bound-identifier=?"
                         (if (id? x) y x)))))
            (set! identifier? (lambda (x) (id? x)))
            (set! identifier->symbol
               (lambda (x)
                  (if (id? x)
                      (void)
                      (call-error
                         'identifier->symbol
                         "non-identifier argument"
                         x))
                  (id-name x)))
            (set! syntax-error
               (lambda (message object)
                  (if (string? message)
                      (syntax-err message (strip object))
                      (call-error
                         'syntax-error
                         "non-string argument"
                         message))))
            (set! boot-install-global-transformer
               (lambda (x e) (install-macro-transformer x e)))
            (set! boot-make-global-syntax
               (lambda (x) (wrap x (make-top-env)))))
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)
         (void)))
     '()
     cons
     car
     cdr
     car
     cadr
     cddr
     cdr
     cdr
     car
     cdr)))
