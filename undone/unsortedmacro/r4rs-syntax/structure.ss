;;; structure.ss
;;; Robert Hieb & Kent Dybvig
;;; 91/01/02
;;; adapted from "The Scheme Programming Language" p. 186

;;; (define-structure (name id1 ...))
;;; (define-structure (name id1 ...) ([id2 val] ...))
(define-syntax define-structure
 (lambda (e)
    (syntax-case (e e)
     [(define-structure (#t #t ...))
      (quasisyntax (define-structure ,(cadr e) ()))]
     [(define-structure (#t #t ...) ((#t #t) ...))
      (let ([name (caadr e)]
            [ids1 (cdadr e)]
            [ids2 (map car (caddr e))]
            [inits (map cadr (caddr e))])
         (let ([ids (append ids1 ids2)])
          (let ([name-str (symbol->string (identifier->symbol name))]
                [id-strs (map symbol->string (map identifier->symbol ids))])
            (let ([constructor (construct-identifier name
                                  (string->symbol (string-append "make-" name-str)))]
                  [predicate (construct-identifier name
                                (string->symbol (string-append name-str "?")))]
                  [access (map (lambda (x)
                                  (construct-identifier name
                                     (string->symbol (string-append name-str "-" x))))
                               id-strs)]
                  [assign (map (lambda (x)
                                  (construct-identifier name
                                     (string->symbol (string-append "set-" name-str "-" x "!"))))
                               id-strs)]
                  [count (+ (length ids) 1)])
               (quasisyntax
                  (begin (define ,predicate
                            (lambda (x)
                               (and (vector? x)
                                    (= (vector-length x) ,count)
                                    (eq? (vector-ref x 0)
                                         (quote ,(identifier->symbol name))))))
                         ,@(let f ([i 1] [ls access])
                              (if (null? ls)
                                  '()
                                  (cons (quasisyntax
                                           (define ,(car ls)
                                              (lambda (x)
                                                 (vector-ref x ,i))))
                                        (f (+ i 1) (cdr ls)))))
                         ,@(let f ([i 1] [ls assign])
                              (if (null? ls)
                                  '()
                                  (cons (quasisyntax
                                           (define ,(car ls)
                                              (lambda (x v)
                                                 (vector-set! x ,i v))))
                                        (f (+ i 1) (cdr ls)))))
                         (define ,constructor
                            (lambda ,ids1
                               (vector (quote ,(identifier->symbol name))
                                       ,@ids1
                                       ,@inits)))))))))]
     [else (syntax-error "invalid syntax" e)])))
