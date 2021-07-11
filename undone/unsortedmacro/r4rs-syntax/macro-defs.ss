;;; macro-defs.ss
;;; Robert Hieb & Kent Dybvig
;;; 90/10/07

(define-syntax let
   (syntax-rules ()
      ((let ((x v) ...) e1 e2 ...)
       ((lambda (x ...) e1 e2 ...) v ...))
      ((let f ((x v) ...) e1 e2 ...)
       ((let ((f #f))
           (set! f (lambda (x ...) e1 e2 ...))
           f)
        v ...))))

(define-syntax or
   (syntax-rules ()
      ((or e1 e2 e3 ...) (let ((x e1)) (if x x (or e2 e3 ...))))
      ((or e) e)
      ((or) #f)))

(define-syntax and
   (syntax-rules ()
      ((and e1 e2 e3 ...) (if e1 (and e2 e3 ...) #f))
      ((and e) e)
      ((and) #t)))

(define-syntax cond
   ;; follows definition in Section 7.3 of r^3.99rs
   (syntax-rules (else =>)
      ((cond) #f)
      ((cond (else e1 e2 ...))
       (begin e1 e2 ...))
      ((cond (e0 => e1) more ...)
       (let ((x e0))
          (if x
              (e1 x)
              (cond more ...))))
      ((cond (e0) more ...)
       (or e0 (cond more ...)))
      ((cond (e0 e1 e2 ...) more ...)
       (if e0 (begin e1 e2 ...) (cond more ...)))))

(define-syntax let*
   (syntax-rules ()
      ((let* () e1 e2 ...)
       (let () e1 e2 ...))
      ((let* ((x1 v1) (x2 v2) ...) e1 e2 ...)
       (let ((x1 v1)) (let* ((x2 v2) ...) e1 e2 ...)))))

(define-syntax case
   ;; follows definition in Section 7.3 of r^3.99rs
   (syntax-rules (else)
      ((case v) v)
      ((case v (else e1 e2 ...))
       (begin e1 e2 ...))
      ((case v ((k ...) e1 e2 ...) more ...)
       (let ((x v))
          (if (memv x '(k ...))
              (begin e1 e2 ...)
              (case x more ...))))))

; used by do and quasiquote
(define-syntax syntax-case
   (syntax-rules (else)
      ((syntax-case (x v))
       v)
      ((syntax-case (x v) (else e))
       (let ((x v)) e))
      ((syntax-case (x v) (p e) m ...)
       (syntax-dispatch v
                        (syntax p)
                        (lambda (x) e)
                        (lambda (y) (syntax-case (x y) m ...))))
      ((syntax-case (x v) (p f e) m ...)
       (let ((fail (lambda (y) (syntax-case (x y) m ...))))
          (syntax-dispatch v
                           (syntax p)
                           (lambda (x) (if f e (fail x)))
                           fail)))))

; this must be done the hard way because of the optional updates
(define-syntax do
   (lambda (x)
      (syntax-case (x x)
         ((do ((#t #t #t ...) ...) (#t #t ...) #t ...)
          (not (memq #f (map (lambda (x) (< (length x) 4)) (cadr x))))
          (let ((vars (map car (cadr x)))
                (inits (map cadr (cadr x)))
                (updates (map (lambda (x)
                                 (if (null? (cddr x))
                                     (car x)
                                     (caddr x)))
                              (cadr x)))
                (test (caaddr x))
                (result (cdaddr x))
                (effects (cdddr x)))
             (quasisyntax
                (let f ,(map list vars inits)
                   (if ,test
                       ,(if (null? result) #f (quasisyntax (begin ,@result)))
                       (begin ,@effects (f ,@updates)))))))
        (else (syntax-error "invalid-syntax" x)))))
