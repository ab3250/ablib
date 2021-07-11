(define-syntax let
  (syntax-rules ()
    ((let tag ((id value) ...)
       body ...)
     ((rec tag (lambda (id ...)
                 body ...))
      value ...))))
