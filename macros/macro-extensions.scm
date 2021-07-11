(define sc-macro-transformer
  (lambda (f)
    (lambda (expr use-env mac-env)
      (make-syntactic-closure mac-env '() (f expr use-env)))))

(define rsc-macro-transformer
  (lambda (f)
    (lambda (expr use-env mac-env)
      (f expr mac-env))))


(define er-macro-transformer
  (lambda (f)
    (lambda (expr use-env mac-env)
      (let ((rename 
              (let ((renames '()))
                (lambda (identifier)
                  (let ((cell (assq identifier renames)))
                    (if cell
                        (cdr cell)
                        (let ((name (make-syntactic-closure mac-env '() identifier)))
                          (set! renames (cons (cons identifier name) renames))
                          name))))))
            (compare 
              (lambda (x y) (identifier=? use-env x use-env y))))
        (f expr rename compare)))))
