;;; quasisyntax.ss
;;; Robert Hieb & Kent Dybvig
;;; 90/10/07

(define-syntax quasisyntax

   (letrec

      ((gen-cons
        (lambda (x y)
           (syntax-case (x x)
              ((syntax #t)
               (syntax-case (y y)
                  ((syntax #t)
                   (quasisyntax (syntax (,(cadr x) . ,(cadr y))))) 
                  ((list #t ...)
                   (quasisyntax (list ,x ,@(cdr y))))
                  (else
                   (quasisyntax (cons ,x ,y)))))
              ((quote #t)
               (syntax-case (y y)
                  ((quote #t)
                   (quasisyntax (quote (,(cadr x) . ,(cadr y))))) 
                  ((list #t ...)
                   (quasisyntax (list ,x ,@(cdr y))))
                  (else
                   (quasisyntax (cons ,x ,y)))))
              (else
               (syntax-case (y y)
                  ((quote ())
                   (quasisyntax (list ,x))) 
                  ((syntax ())
                   (quasisyntax (list ,x))) 
                  ((list #t ...)
                   (quasisyntax (list ,x ,@(cdr y))))
                  (else
                   (quasisyntax (cons ,x ,y))))))))

       (gen-vector
        (lambda (x)
           (syntax-case (x x)
              ((quote #t)
               (quasisyntax (quote #(,@(cadr x)))))
              ((syntax #t)
               (quasisyntax (syntax #(,@(cadr x)))))
              ((list #t ...)
               (quasisyntax (vector ,@(cdr x))))
              (else
               (quasisyntax (list->vector ,x))))))

       (gen-syntax
        (lambda (x)
           (quasisyntax (syntax ,x))))

       (gen-quote
        (lambda (x)
           (quasisyntax (quote ,x))))

       (gen
        (lambda (p gen-type lev exp)
           (syntax-case (p p)
              ((unquote #t)
               (if (null? lev)
                   (cadr p)
                   (gen-cons (gen-type (car p))
                             (gen (cdr p) (car lev) (cdr lev) exp))))
              (((unquote-splicing #t))
               (if (null? lev)
                   (cadar p)
                   (gen-cons (gen-cons (gen-type (caar p))
                                       (gen (cdar p) (car lev) (cdr lev) exp))
                             (gen (cdr p) gen-type lev exp))))
              (((unquote-splicing #t) . #t)
               (if (null? lev)
                   (quasisyntax (append ,(cadar p)
                                        ,(gen (cdr p) gen-type lev exp)))
                   (gen-cons (gen-cons (gen-type (caar p))
                                       (gen (cdar p) (car lev) (cdr lev) exp))
                             (gen (cdr p) gen-type lev exp))))
              ((quasisyntax #t)
               (gen-cons (gen-type (car p))
                         (gen (cdr p) gen-syntax (cons gen-type lev) exp)))
              ((quasiquote #t)
               (gen-cons (gen-type (car p))
                         (gen (cdr p) gen-quote (cons gen-type lev) exp)))
              ((#t . #t)
               (gen-cons (gen (car p) gen-type lev exp)
                         (gen (cdr p) gen-type lev exp)))
              (unquote
               (syntax-error "invalid use of unquote" exp))
              (unquote-splicing
               (syntax-error "invalid use of unquote-splicing" exp))
              (quasiquote
               (syntax-error "invalid use of quasiquote" exp))
              (quasisyntax
               (syntax-error "invalid use of quasisyntax" exp))
              (#t
               (identifier? p)
               (gen-type p))
              (#(#t ...)
               (gen-vector (gen (vector->list p) gen-type lev exp)))
              (else
               (gen-type p))))))

    (lambda (x)
       (syntax-case (x x)
          ((quasisyntax #t) (gen (cadr x) gen-syntax '() x))
          (else (syntax-error "invalid syntax" x))))))
