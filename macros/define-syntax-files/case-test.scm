;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


((lambda()(case #f
    ((= x #t)(display "true"))
      ((= x #f)(display "false")))))

        (case a
          ((#\J)
           (case b
             ((#\a) (case c ((#\n) 1) (else (bad))))
             ((#\u) (case c ((#\n) 6) ((#\l) 7) (else (bad))))
             (else (bad))))
          ((#\F)
           (case b
             ((#\e) (case c ((#\b) 2) (else (bad))))
             (else (bad))))
          ((#\M)
           (case b
             ((#\a) (case c ((#\r) 3) ((#\y) 5) (else (bad))))
             (else (bad))))
          ((#\A)
           (case b
             ((#\p) (case c ((#\r) 4) (else (bad))))
             ((#\u) (case c ((#\g) 8) (else (bad))))
             (else (bad))))
          ((#\S)
           (case b
             ((#\e) (case c ((#\p) 9) (else (bad))))
             (else (bad))))
          ((#\O)
           (case b
             ((#\c) (case c ((#\t) 10) (else (bad))))
             (else (bad))))
          ((#\N)
           (case b
             ((#\o) (case c ((#\v) 11) (else (bad))))
             (else (bad))))
          ((#\D)
           (case b
             ((#\e) (case c ((#\c) 12) (else (bad))))
             (else (bad))))
          (else (bad))))))
