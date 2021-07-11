(boot-install-global-transformer
   'quasisyntax
   ((lambda (gen-cons gen-vector gen-syntax gen-quote gen)
       (set! gen-cons
          (lambda (x y)
             (syntax-dispatch
                x
                (boot-make-global-syntax '(syntax #t))
                (lambda (x)
                   (syntax-dispatch
                      y
                      (boot-make-global-syntax '(syntax #t))
                      (lambda (y)
                         (cons (boot-make-global-syntax 'syntax)
                               (cons (cons (cadr x) (cadr y))
                                     (boot-make-global-syntax '()))))
                      (lambda (y)
                         (syntax-dispatch
                            y
                            (boot-make-global-syntax '(list #t ...))
                            (lambda (y)
                               (cons (boot-make-global-syntax 'list)
                                     (cons x (cdr y))))
                            (lambda (y)
                               ((lambda (y)
                                   (cons (boot-make-global-syntax 'cons)
                                         (cons x
                                               (cons y
                                                     (boot-make-global-syntax
                                                        '())))))
                                y))))))
                (lambda (x)
                   (syntax-dispatch
                      x
                      (boot-make-global-syntax ''#t)
                      (lambda (x)
                         (syntax-dispatch
                            y
                            (boot-make-global-syntax ''#t)
                            (lambda (y)
                               (cons (boot-make-global-syntax 'quote)
                                     (cons (cons (cadr x) (cadr y))
                                           (boot-make-global-syntax '()))))
                            (lambda (y)
                               (syntax-dispatch
                                  y
                                  (boot-make-global-syntax '(list #t ...))
                                  (lambda (y)
                                     (cons (boot-make-global-syntax 'list)
                                           (cons x (cdr y))))
                                  (lambda (y)
                                     ((lambda (y)
                                         (cons (boot-make-global-syntax
                                                  'cons)
                                               (cons x
                                                     (cons y
                                                           (boot-make-global-syntax
                                                              '())))))
                                      y))))))
                      (lambda (x)
                         ((lambda (x)
                             (syntax-dispatch
                                y
                                (boot-make-global-syntax ''())
                                (lambda (y)
                                   (cons (boot-make-global-syntax 'list)
                                         (cons x
                                               (boot-make-global-syntax
                                                  '()))))
                                (lambda (y)
                                   (syntax-dispatch
                                      y
                                      (boot-make-global-syntax
                                         '(syntax ()))
                                      (lambda (y)
                                         (cons (boot-make-global-syntax
                                                  'list)
                                               (cons x
                                                     (boot-make-global-syntax
                                                        '()))))
                                      (lambda (y)
                                         (syntax-dispatch
                                            y
                                            (boot-make-global-syntax
                                               '(list #t ...))
                                            (lambda (y)
                                               (cons (boot-make-global-syntax
                                                        'list)
                                                     (cons x (cdr y))))
                                            (lambda (y)
                                               ((lambda (y)
                                                   (cons (boot-make-global-syntax
                                                            'cons)
                                                         (cons x
                                                               (cons y
                                                                     (boot-make-global-syntax
                                                                        '())))))
                                                y))))))))
                          x)))))))
       (set! gen-vector
          (lambda (x)
             (syntax-dispatch
                x
                (boot-make-global-syntax ''#t)
                (lambda (x)
                   (cons (boot-make-global-syntax 'quote)
                         (cons (list->vector (cadr x))
                               (boot-make-global-syntax '()))))
                (lambda (x)
                   (syntax-dispatch
                      x
                      (boot-make-global-syntax '(syntax #t))
                      (lambda (x)
                         (cons (boot-make-global-syntax 'syntax)
                               (cons (list->vector (cadr x))
                                     (boot-make-global-syntax '()))))
                      (lambda (x)
                         (syntax-dispatch
                            x
                            (boot-make-global-syntax '(list #t ...))
                            (lambda (x)
                               (cons (boot-make-global-syntax 'vector)
                                     (cdr x)))
                            (lambda (x)
                               ((lambda (x)
                                   (cons (boot-make-global-syntax
                                            'list->vector)
                                         (cons x
                                               (boot-make-global-syntax
                                                  '()))))
                                x)))))))))
       (set! gen-syntax
          (lambda (x)
             (cons (boot-make-global-syntax 'syntax)
                   (cons x (boot-make-global-syntax '())))))
       (set! gen-quote
          (lambda (x)
             (cons (boot-make-global-syntax 'quote)
                   (cons x (boot-make-global-syntax '())))))
       (set! gen
          (lambda (p gen-type lev exp)
             (syntax-dispatch
                p
                (boot-make-global-syntax ',#t)
                (lambda (p)
                   (if (null? lev)
                       (cadr p)
                       (gen-cons
                          (gen-type (car p))
                          (gen (cdr p) (car lev) (cdr lev) exp))))
                (lambda (p)
                   (syntax-dispatch
                      p
                      (boot-make-global-syntax '(,@#t))
                      (lambda (p)
                         (if (null? lev)
                             (cadar p)
                             (gen-cons
                                (gen-cons
                                   (gen-type (caar p))
                                   (gen (cdar p) (car lev) (cdr lev) exp))
                                (gen (cdr p) gen-type lev exp))))
                      (lambda (p)
                         (syntax-dispatch
                            p
                            (boot-make-global-syntax '(,@#t . #t))
                            (lambda (p)
                               (if (null? lev)
                                   (cons (boot-make-global-syntax 'append)
                                         (cons (cadar p)
                                               (cons (gen (cdr p)
                                                          gen-type
                                                          lev
                                                          exp)
                                                     (boot-make-global-syntax
                                                        '()))))
                                   (gen-cons
                                      (gen-cons
                                         (gen-type (caar p))
                                         (gen (cdar p)
                                              (car lev)
                                              (cdr lev)
                                              exp))
                                      (gen (cdr p) gen-type lev exp))))
                            (lambda (p)
                               (syntax-dispatch
                                  p
                                  (boot-make-global-syntax
                                     '(quasisyntax #t))
                                  (lambda (p)
                                     (gen-cons
                                        (gen-type (car p))
                                        (gen (cdr p)
                                             gen-syntax
                                             (cons gen-type lev)
                                             exp)))
                                  (lambda (p)
                                     (syntax-dispatch
                                        p
                                        (boot-make-global-syntax '`#t)
                                        (lambda (p)
                                           (gen-cons
                                              (gen-type (car p))
                                              (gen (cdr p)
                                                   gen-quote
                                                   (cons gen-type lev)
                                                   exp)))
                                        (lambda (p)
                                           (syntax-dispatch
                                              p
                                              (boot-make-global-syntax
                                                 '(#t . #t))
                                              (lambda (p)
                                                 (gen-cons
                                                    (gen (car p)
                                                         gen-type
                                                         lev
                                                         exp)
                                                    (gen (cdr p)
                                                         gen-type
                                                         lev
                                                         exp)))
                                              (lambda (p)
                                                 (syntax-dispatch
                                                    p
                                                    (boot-make-global-syntax
                                                       'unquote)
                                                    (lambda (p)
                                                       (syntax-error
                                                          "invalid use of unquote"
                                                          exp))
                                                    (lambda (p)
                                                       (syntax-dispatch
                                                          p
                                                          (boot-make-global-syntax
                                                             'unquote-splicing)
                                                          (lambda (p)
                                                             (syntax-error
                                                                "invalid use of unquote-splicing"
                                                                exp))
                                                          (lambda (p)
                                                             (syntax-dispatch
                                                                p
                                                                (boot-make-global-syntax
                                                                   'quasiquote)
                                                                (lambda (p)
                                                                   (syntax-error
                                                                      "invalid use of quasiquote"
                                                                      exp))
                                                                (lambda (p)
                                                                   (syntax-dispatch
                                                                      p
                                                                      (boot-make-global-syntax
                                                                         'quasisyntax)
                                                                      (lambda (p)
                                                                         (syntax-error
                                                                            "invalid use of quasisyntax"
                                                                            exp))
                                                                      (lambda (p)
                                                                         ((lambda (exp
                                                                                   fender
                                                                                   body
                                                                                   rest)
                                                                             (syntax-dispatch
                                                                                exp
                                                                                (boot-make-global-syntax
                                                                                   '#t)
                                                                                (lambda (u)
                                                                                   (if (fender
                                                                                          u)
                                                                                       (body u)
                                                                                       (rest u)))
                                                                                rest))
                                                                          p
                                                                          (lambda (p)
                                                                             (identifier?
                                                                                p))
                                                                          (lambda (p)
                                                                             (gen-type
                                                                                p))
                                                                          (lambda (p)
                                                                             (syntax-dispatch
                                                                                p
                                                                                (boot-make-global-syntax
                                                                                   '#(#t
                                                                                      ...))
                                                                                (lambda (p)
                                                                                   (gen-vector
                                                                                      (gen (vector->list
                                                                                              p)
                                                                                           gen-type
                                                                                           lev
                                                                                           exp)))
                                                                                (lambda (p)
                                                                                   ((lambda (p)
                                                                                       (gen-type
                                                                                          p))
                                                                                    p)))))))))))))))))))))))))))
       (lambda (x)
          (syntax-dispatch
             x
             (boot-make-global-syntax '(quasisyntax #t))
             (lambda (x) (gen (cadr x) gen-syntax '() x))
             (lambda (x)
                ((lambda (x) (syntax-error "invalid syntax" x)) x)))))
    (void)
    (void)
    (void)
    (void)
    (void)))
