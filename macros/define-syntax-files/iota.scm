;;n to list of numbers

(define-syntax n->lon
  (lambda (stx)
    (syntax-case stx ()
      ((n->lon 0 (n ...))
       #'(list n ...))
      ((n->lon n (m ...))
       #`(n->lon #,(- (syntax->datum #'n) 1) (n m ...)))
      ((n->lon n)
       #`(n->lon #,(- (syntax->datum #'n) 1) ())))))
