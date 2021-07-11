(define-syntax rec
  (syntax-rules ()
    ((rec name value)
     (let ()
       (define name value)
       name))))

(define-syntax rec
  (syntax-rules ()
    ((rec name value)
     (letrec ((name value))
       name))))

