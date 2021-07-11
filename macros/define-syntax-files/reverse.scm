(define-syntax reverse-me
    (lambda (stx)
        (datum->syntax stx (reverse (cdr (syntax->datum stx))))))
