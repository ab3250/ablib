;;; quasiquote.ss
;;; Robert Hieb & Kent Dybvig
;;; 90/10/07

(define-syntax quasiquote

   (letrec

      ((gen-cons
        (lambda (x y)
           (syntax-case (x x)
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
                  ((list #t ...)
                   (quasisyntax (list ,x ,@(cdr y))))
                  (else
                   (quasisyntax (cons ,x ,y))))))))

       (gen-vector
        (lambda (x)
           (syntax-case (x x)
              ((quote (#t ...))
               (quasisyntax (quote #(,@(cadr x)))))
              ((list #t ...)
               (quasisyntax (vector ,@(cdr x))))
              (else
               (quasisyntax (list->vector ,x))))))

       (gen
        (lambda (p lev exp)
           (syntax-case (p p)
              ((unquote #t)
               (if (= lev 0)
                   (cadr p)
                   (gen-cons (syntax (quote unquote))
                             (gen (cdr p) (- lev 1) exp))))
              (((unquote-splicing #t))
               (if (= lev 0)
                   (cadar p)
                   (gen-cons (gen-cons (syntax (quote unquote-splicing))
                                       (gen (cdar p) (- lev 1) exp))
                             (gen (cdr p) lev exp))))
              (((unquote-splicing #t) . #t)
               (if (= lev 0)
                   (quasisyntax (append ,(cadar p) ,(gen (cdr p) lev exp)))
                   (gen-cons (gen-cons (syntax (quote unquote-splicing))
                                       (gen (cdar p) (- lev 1) exp))
                             (gen (cdr p) lev exp))))
              ((quasiquote #t)
               (gen-cons (syntax (quote quasiquote))
                         (gen (cdr p) (+ lev 1) exp)))
              ((#t . #t)
               (gen-cons (gen (car p) lev exp)
                         (gen (cdr p) lev exp)))
              (unquote
               (syntax-error "invalid use of unquote" exp))
              (unquote-splicing
               (syntax-error "invalid use of unquote-splicing" exp))
              (quasiquote
               (syntax-error "invalid use of quasiquote" exp))
              (#t
               (identifier? p)
               (quasisyntax (quote ,p)))
              (#(#t ...)
               (gen-vector (gen (vector->list p) lev exp)))
              (else
               (quasisyntax (quote ,p)))))))

    (lambda (x)
       (syntax-case (x x)
          ((quasiquote #t) (gen (cadr x) 0 x))
          (else (syntax-error "invalid syntax" x))))))
