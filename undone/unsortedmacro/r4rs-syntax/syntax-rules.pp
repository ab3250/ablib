(boot-install-global-transformer
   'syntax-rules
   ((lambda (id-pattern
             id-access
             id-control
             loop-controls
             set-loop-controls!)
       ((lambda (make-id
                 start-loop
                 key?
                 key-check
                 ellipsis?
                 gen-input
                 make-ids
                 gen-output
                 lookup
                 add-control!
                 gen-loop
                 gen-syntax
                 gen-clauses)
           (set! make-id (lambda (pat acc cntl) (list pat acc cntl)))
           (set! start-loop (lambda () (list '())))
           (set! key?
              (lambda (i keys)
                 ((lambda (g0005)
                     (if g0005
                         ((lambda (g0006)
                             (if g0006 g0006 (key? i (cdr keys))))
                          (bound-identifier=? i (car keys)))
                         g0005))
                  (not (null? keys)))))
           (set! key-check
              (lambda (keys)
                 ((lambda (g0007)
                     (if g0007
                         g0007
                         ((lambda (g0008)
                             (if g0008
                                 ((lambda (g0009)
                                     (if g0009
                                         (key-check (cdr keys))
                                         g0009))
                                  (not (ellipsis? (car keys))))
                                 g0008))
                          (identifier? (car keys)))))
                  (null? keys))))
           (set! ellipsis?
              (lambda (x)
                 ((lambda (g00010)
                     (if g00010
                         (free-identifier=?
                            x
                            (boot-make-global-syntax '...))
                         g00010))
                  (identifier? x))))
           (set! gen-input
              (lambda (keys pat err)
                 (((lambda (gen)
                      (set! gen
                         (lambda (p)
                            ((lambda (exp fender body rest)
                                (syntax-dispatch
                                   exp
                                   (boot-make-global-syntax '(#t #t))
                                   (lambda (u)
                                      (if (fender u) (body u) (rest u)))
                                   rest))
                             p
                             (lambda (p) (ellipsis? (cadr p)))
                             (lambda (p) (cons (gen (car p)) (cdr p)))
                             (lambda (p)
                                (syntax-dispatch
                                   p
                                   (boot-make-global-syntax '(#t . #t))
                                   (lambda (p)
                                      (cons (gen (car p)) (gen (cdr p))))
                                   (lambda (p)
                                      (syntax-dispatch
                                         p
                                         (boot-make-global-syntax '())
                                         (lambda (p) p)
                                         (lambda (p)
                                            ((lambda (p)
                                                (if (identifier? p)
                                                    (if (key? p keys)
                                                        p
                                                        (if (ellipsis? p)
                                                            (err "misplaced ellipsis in syntax-rules pattern"
                                                                 pat)
                                                            #t))
                                                    (err "invalid syntax-rules pattern"
                                                         pat)))
                                             p)))))))))
                      gen)
                   (void))
                  pat)))
           (set! make-ids
              (lambda (keys pat err acc cntl ids)
                 ((lambda (exp fender body rest)
                     (syntax-dispatch
                        exp
                        (boot-make-global-syntax '(#t #t))
                        (lambda (u) (if (fender u) (body u) (rest u)))
                        rest))
                  pat
                  (lambda (pat) (ellipsis? (cadr pat)))
                  (lambda (pat)
                     ((lambda (x)
                         (make-ids
                            keys
                            (car pat)
                            err
                            x
                            (make-id x acc cntl)
                            ids))
                      (generate-identifier 'x)))
                  (lambda (pat)
                     (syntax-dispatch
                        pat
                        (boot-make-global-syntax '(#t . #t))
                        (lambda (pat)
                           (make-ids
                              keys
                              (car pat)
                              err
                              (cons (boot-make-global-syntax 'car)
                                    (cons acc
                                          (boot-make-global-syntax '())))
                              cntl
                              (make-ids
                                 keys
                                 (cdr pat)
                                 err
                                 (cons (boot-make-global-syntax 'cdr)
                                       (cons acc
                                             (boot-make-global-syntax
                                                '())))
                                 cntl
                                 ids)))
                        (lambda (pat)
                           ((lambda (pat)
                               (if ((lambda (g00011)
                                       (if g00011
                                           (not (key? pat keys))
                                           g00011))
                                    (identifier? pat))
                                   (if (lookup pat ids)
                                       (err "duplicate syntax-rules pattern variable"
                                            pat)
                                       (cons (make-id pat acc cntl) ids))
                                   ids))
                            pat)))))))
           (set! gen-output
              (lambda (pat ids err)
                 (((lambda (gen)
                      (set! gen
                         (lambda (p loops)
                            (if (identifier? p)
                                ((lambda (id)
                                    (if id
                                        ((lambda ()
                                            (add-control!
                                               pat
                                               (id-control id)
                                               loops
                                               err)
                                            (list (boot-make-global-syntax
                                                     'unquote)
                                                  (id-access id))))
                                        (if (ellipsis? p)
                                            (err "misplaced ellipsis in syntax-rules pattern"
                                                 pat)
                                            (if ((lambda (g00012)
                                                    (if g00012
                                                        g00012
                                                        ((lambda (g00013)
                                                            (if g00013
                                                                g00013
                                                                ((lambda (g00014)
                                                                    (if g00014
                                                                        g00014
                                                                        (free-identifier=?
                                                                           p
                                                                           (boot-make-global-syntax
                                                                              'syntax))))
                                                                 (free-identifier=?
                                                                    p
                                                                    (boot-make-global-syntax
                                                                       'quasiquote)))))
                                                         (free-identifier=?
                                                            p
                                                            (boot-make-global-syntax
                                                               'unquote-splicing)))))
                                                 (free-identifier=?
                                                    p
                                                    (boot-make-global-syntax
                                                       'unquote)))
                                                (cons (boot-make-global-syntax
                                                         'unquote)
                                                      (cons (cons (boot-make-global-syntax
                                                                     'syntax)
                                                                  (cons p
                                                                        (boot-make-global-syntax
                                                                           '())))
                                                            (boot-make-global-syntax
                                                               '())))
                                                p))))
                                 (lookup p ids))
                                ((lambda (exp fender body rest)
                                    (syntax-dispatch
                                       exp
                                       (boot-make-global-syntax
                                          '(#t #t . #t))
                                       (lambda (u)
                                          (if (fender u)
                                              (body u)
                                              (rest u)))
                                       rest))
                                 p
                                 (lambda (p) (ellipsis? (cadr p)))
                                 (lambda (p)
                                    (cons (list (boot-make-global-syntax
                                                   'unquote-splicing)
                                                ((lambda (l)
                                                    (gen-loop
                                                       pat
                                                       l
                                                       (gen (car p)
                                                            (cons l loops))
                                                       err))
                                                 (start-loop)))
                                          (gen (cddr p) loops)))
                                 (lambda (p)
                                    (syntax-dispatch
                                       p
                                       (boot-make-global-syntax '(#t . #t))
                                       (lambda (p)
                                          (cons (gen (car p) loops)
                                                (gen (cdr p) loops)))
                                       (lambda (p)
                                          ((lambda (p) p) p))))))))
                      gen)
                   (void))
                  pat
                  '())))
           (set! lookup
              (lambda (i ids)
                 ((lambda (g00015)
                     (if g00015
                         (if (bound-identifier=? i (id-pattern (car ids)))
                             (car ids)
                             (lookup i (cdr ids)))
                         g00015))
                  (not (null? ids)))))
           (set! add-control!
              (lambda (pat control loops err)
                 ((lambda (g00016)
                     (if g00016
                         g00016
                         (if (null? loops)
                             (err "missing ellipsis in syntax-rules output pattern"
                                  pat)
                             ((lambda ()
                                 ((lambda (ids)
                                     (if (not (memq control ids))
                                         (set-loop-controls!
                                            (car loops)
                                            (cons control ids))
                                         (void)))
                                  (loop-controls (car loops)))
                                 (add-control!
                                    pat
                                    (id-control control)
                                    (cdr loops)
                                    err))))))
                  (null? control))))
           (set! gen-loop
              (lambda (pat loop body err)
                 ((lambda (ids)
                     (if (null? ids)
                         (err "extra ellipsis in syntax-rules output pattern"
                              pat)
                         ((lambda (exp fender body rest)
                             (syntax-dispatch
                                exp
                                (boot-make-global-syntax ',#t)
                                (lambda (u)
                                   (if (fender u) (body u) (rest u)))
                                rest))
                          body
                          (lambda (body)
                             ((lambda (i)
                                 ((lambda (g00019)
                                     (if g00019
                                         (bound-identifier=?
                                            (id-pattern (car ids))
                                            i)
                                         g00019))
                                  (identifier? i)))
                              (cadr body)))
                          (lambda (body) (id-access (car ids)))
                          (lambda (body)
                             ((lambda (exp fender body rest)
                                 (syntax-dispatch
                                    exp
                                    (boot-make-global-syntax ',(#t #t))
                                    (lambda (u)
                                       (if (fender u) (body u) (rest u)))
                                    rest))
                              body
                              (lambda (body)
                                 ((lambda (g00017)
                                     (if g00017
                                         ((lambda (i)
                                             ((lambda (g00018)
                                                 (if g00018
                                                     (bound-identifier=?
                                                        (id-pattern
                                                           (car ids))
                                                        i)
                                                     g00018))
                                              (identifier? i)))
                                          (cadadr body))
                                         g00017))
                                  (null? (cdr ids))))
                              (lambda (body)
                                 (cons (boot-make-global-syntax 'map)
                                       (cons (caadr body)
                                             (cons (id-access (car ids))
                                                   (boot-make-global-syntax
                                                      '())))))
                              (lambda (body)
                                 ((lambda (body)
                                     (cons (boot-make-global-syntax 'map)
                                           (cons (cons (boot-make-global-syntax
                                                          'lambda)
                                                       (cons (map id-pattern
                                                                  ids)
                                                             (cons (gen-syntax
                                                                      body)
                                                                   (boot-make-global-syntax
                                                                      '()))))
                                                 (map id-access ids))))
                                  body)))))))
                  (loop-controls loop))))
           (set! gen-syntax
              (lambda (x)
                 (list (boot-make-global-syntax 'quasisyntax) x)))
           (set! gen-clauses
              (lambda (source keys clauses err)
                 (if (null? clauses)
                     (cons (boot-make-global-syntax 'syntax-error)
                           (cons (boot-make-global-syntax
                                    '"invalid syntax")
                                 (cons source
                                       (boot-make-global-syntax '()))))
                     ((lambda (input output rest)
                         (cons (boot-make-global-syntax 'syntax-dispatch)
                               (cons source
                                     (cons (cons (boot-make-global-syntax
                                                    'syntax)
                                                 (cons (gen-input
                                                          keys
                                                          input
                                                          err)
                                                       (boot-make-global-syntax
                                                          '())))
                                           (cons (cons (boot-make-global-syntax
                                                          'lambda)
                                                       (cons (cons (boot-make-global-syntax
                                                                      'x)
                                                                   (boot-make-global-syntax
                                                                      '()))
                                                             (cons (gen-syntax
                                                                      (gen-output
                                                                         output
                                                                         (make-ids
                                                                            keys
                                                                            input
                                                                            err
                                                                            (boot-make-global-syntax
                                                                               'x)
                                                                            '()
                                                                            '())
                                                                         err))
                                                                   (boot-make-global-syntax
                                                                      '()))))
                                                 (cons (cons (boot-make-global-syntax
                                                                'lambda)
                                                             (cons (cons (boot-make-global-syntax
                                                                            'x)
                                                                         (boot-make-global-syntax
                                                                            '()))
                                                                   (cons (gen-clauses
                                                                            (boot-make-global-syntax
                                                                               'x)
                                                                            keys
                                                                            rest
                                                                            err)
                                                                         (boot-make-global-syntax
                                                                            '()))))
                                                       (boot-make-global-syntax
                                                          '())))))))
                      (caar clauses)
                      (cadar clauses)
                      (cdr clauses)))))
           (lambda (x)
              ((lambda (exp fender body rest)
                  (syntax-dispatch
                     exp
                     (boot-make-global-syntax
                        '(syntax-rules (#t ...) (#t #t) ...))
                     (lambda (u) (if (fender u) (body u) (rest u)))
                     rest))
               x
               (lambda (x) (key-check (cadr x)))
               (lambda (x)
                  (call-with-current-continuation
                     (lambda (k)
                        (cons (boot-make-global-syntax 'lambda)
                              (cons (cons (boot-make-global-syntax 'exp)
                                          (boot-make-global-syntax '()))
                                    (cons (gen-clauses
                                             (boot-make-global-syntax 'exp)
                                             (cadr x)
                                             (cddr x)
                                             (lambda (m x)
                                                (k (syntax-error m x))))
                                          (boot-make-global-syntax
                                             '())))))))
               (lambda (x)
                  ((lambda (x) (syntax-error "invalid syntax" x)) x)))))
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
    car
    cadr
    caddr
    car
    set-car!))
