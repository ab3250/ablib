((lambda ()
    (set! syntax-dispatch
       (lambda (expression pattern success fail)
          (((lambda (dispatch)
               (set! dispatch
                  (lambda (e p k)
                     (if (eq? p #t)
                         (k e)
                         (if (identifier? p)
                             (if ((lambda (g0008)
                                     (if g0008
                                         (free-identifier=? e p)
                                         g0008))
                                  (identifier? e))
                                 (k e)
                                 (fail expression))
                             (if (pair? p)
                                 ((lambda (patcar patcdr)
                                     (if ((lambda (g0005)
                                             (if g0005
                                                 ((lambda (g0006)
                                                     (if g0006
                                                         ((lambda (g0007)
                                                             (if g0007
                                                                 (null?
                                                                    (unwrap-syntax
                                                                       (cdr patcdr)))
                                                                 g0007))
                                                          (free-identifier=?
                                                             (car patcdr)
                                                             (boot-make-global-syntax
                                                                '...)))
                                                         g0006))
                                                  (identifier?
                                                     (car patcdr)))
                                                 g0005))
                                          (pair? patcdr))
                                         (((lambda (dispatch-list)
                                              (set! dispatch-list
                                                 (lambda (e k)
                                                    (if (pair? e)
                                                        (dispatch
                                                           (car e)
                                                           patcar
                                                           (lambda (a)
                                                              (dispatch-list
                                                                 (unwrap-syntax
                                                                    (cdr e))
                                                                 (lambda (d)
                                                                    (k (cons a
                                                                             d))))))
                                                        (if (null? e)
                                                            (k '())
                                                            (fail expression)))))
                                              dispatch-list)
                                           (void))
                                          (unwrap-syntax e)
                                          k)
                                         ((lambda (e)
                                             (if (pair? e)
                                                 (dispatch
                                                    (car e)
                                                    patcar
                                                    (lambda (a)
                                                       (dispatch
                                                          (cdr e)
                                                          patcdr
                                                          (lambda (d)
                                                             (k (cons a
                                                                      d))))))
                                                 (fail expression)))
                                          (unwrap-syntax e))))
                                  (unwrap-syntax (car p))
                                  (unwrap-syntax (cdr p)))
                                 (if (null? p)
                                     (if (null? (unwrap-syntax e))
                                         (k '())
                                         (fail expression))
                                     (if (vector? p)
                                         ((lambda (e)
                                             (if (vector? e)
                                                 (dispatch
                                                    (vector->list e)
                                                    (vector->list p)
                                                    (lambda (x)
                                                       (k (list->vector
                                                             x))))
                                                 (fail expression)))
                                          (unwrap-syntax e))
                                         (equal?
                                            p
                                            (unwrap-syntax e)))))))))
               dispatch)
            (void))
           expression
           (unwrap-syntax pattern)
           success)))
    'syntax-dispatch))
