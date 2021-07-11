 (define-syntax cond
    (lambda (x)
      (syntax-case x ()
        ((_ c1 c2 ...)
         (let f ((c1  (syntax c1))
                 (c2* (syntax (c2 ...))))
           (syntax-case c2* ()
             (()
              (syntax-case c1 (else =>)
                ((else e1 e2 ...) (syntax (begin e1 e2 ...)))
                ((e0)             (syntax (let ((t e0)) (if t t))))
                ((e0 => e1)       (syntax (let ((t e0)) (if t (e1 t)))))
                ((e0 e1 e2 ...)   (syntax (if e0 (begin e1 e2 ...))))
                (_                (syntax-violation 'cond "Invalid expression" x))))
             ((c2 c3 ...)
              (with-syntax ((rest (f (syntax c2)
                                     (syntax (c3 ...)))))
                (syntax-case c1 (else =>)
                  ((e0)           (syntax (let ((t e0)) (if t t rest))))
                  ((e0 => e1)     (syntax (let ((t e0)) (if t (e1 t) rest))))
                  ((e0 e1 e2 ...) (syntax (if e0 (begin e1 e2 ...) rest)))
                  (_              (syntax-violation 'cond "Invalid expression" x)))))))))))

  (define-syntax case
    (lambda (x)
      (syntax-case x ()
        ((_ e c1 c2 ...)
         (with-syntax ((body
                        (let f ((c1 (syntax c1))
                                (cmore (syntax (c2 ...))))
                          (if (null? cmore)
                              (syntax-case c1 (else)
                                ((else e1 e2 ...)    (syntax (begin e1 e2 ...)))
                                (((k ...) e1 e2 ...) (syntax (if (memv t '(k ...))
                                                                 (begin e1 e2 ...)))))
                              (with-syntax ((rest (f (car cmore) (cdr cmore))))
                                (syntax-case c1 ()
                                  (((k ...) e1 e2 ...)
                                   (syntax (if (memv t '(k ...))
                                               (begin e1 e2 ...)
                                               rest)))))))))
           (syntax (let ((t e)) body)))))))
           
   (define-syntax case
     (lambda (x)
       (syntax-case x ()
         ((_ e c1 c2 ...)
          (quasisyntax
           (let ((t e))
             (unsyntax
              (let f ((c1    (syntax c1))
                      (cmore (syntax (c2 ...))))
                (if (null? cmore)
                    (syntax-case c1 (else)
                      ((else e1 e2 ...)    (syntax (begin e1 e2 ...)))
                      (((k ...) e1 e2 ...) (syntax (if (memv t '(k ...))
                                                       (begin e1 e2 ...)))))
                    (syntax-case c1 ()
                      (((k ...) e1 e2 ...)
                       (quasisyntax
                        (if (memv t '(k ...))
                            (begin e1 e2 ...)
                            (unsyntax
                             (f (car cmore) (cdr cmore))))))))))))))))

(define-syntax =>
    (lambda (x)
      (syntax-violation '=> "Invalid expression" x)))

(define-syntax else
    (lambda (x)
      (syntax-violation 'else "Invalid expression" x)))

  ) ; derived
  
  (define-syntax when
    (syntax-rules ()
      ((when test result1 result2 ...)
       (if test
           (begin result1 result2 ...)))))

  (define-syntax unless
    (syntax-rules ()
      ((unless test result1 result2 ...)
       (if (not test)
           (begin result1 result2 ...)))))

  (define-syntax do
    (lambda (orig-x)
      (syntax-case orig-x ()
        ((_ ((var init . step) ...) (e0 e1 ...) c ...)
         (with-syntax (((step ...)
                        (map (lambda (v s)
                               (syntax-case s ()
                                 (()  v)
                                 ((e) (syntax e))
                                 (_   (syntax-violation 'do "Invalid step" orig-x s))))
                             (syntax (var ...))
                             (syntax (step ...)))))
           (syntax-case (syntax (e1 ...)) ()
             (()          (syntax (let do ((var init) ...)
                                    (if (not e0)
                                        (begin c ... (do step ...))))))
             ((e1 e2 ...) (syntax (let do ((var init) ...)
                                    (if e0
                                        (begin e1 e2 ...)
                                        (begin c ... (do step ...))))))))))))
           
