;;; syntax-rules.ss
;;; Robert Hieb & Kent Dybvig
;;; 90/10/07
;;; modified 91/01/02

;;; This code is derived from Chez Scheme's implementation of
;;; extend-syntax, which bears the copyright notice below:

;;; --------
;;; extend.ss
;;; Copyright (C) 1987 R. Kent Dybvig
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright notice in full.

;;; The basic design of extend-syntax is due to Eugene Kohlbecker.  See
;;; "E. Kohlbecker: Syntactic Extensions in the Programming Language Lisp",
;;; Ph.D.  Dissertation, Indiana University, 1986."  The structure of "with"
;;; pattern/value clauses, the method for compiling extend-syntax into
;;; Scheme code, and the actual implementation are due to Kent Dybvig.
;;; --------

(define-syntax syntax-rules

   (letrec

      ((make-id
        (lambda (pat acc cntl)
           (list pat acc cntl)))

       (id-pattern car)

       (id-access cadr)

       (id-control caddr)
  
       (start-loop
        (lambda ()
           (list '())))

       (loop-controls car)

       (set-loop-controls! set-car!)

       (key?
        (lambda (i keys)
           (and (not (null? keys))
                (or (bound-identifier=? i (car keys))
                    (key? i (cdr keys))))))

       (key-check
        (lambda (keys)
           (or (null? keys)
               (and (identifier? (car keys))
                    (not (ellipsis? (car keys)))
                    (key-check (cdr keys))))))
 
       (ellipsis?
        (lambda (x)
           (and (identifier? x)
                (free-identifier=? x (syntax ...)))))

       (gen-input
        (lambda (keys pat err)
           (let gen ((p pat))
              (syntax-case (p p)
                 ((#t #t)
                  (ellipsis? (cadr p))
                  (cons (gen (car p)) (cdr p)))
                 ((#t . #t) (cons (gen (car p)) (gen (cdr p))))
                 (() p)
                 (else
                  (if (identifier? p)
                      (cond
                         ((key? p keys) p)
                         ((ellipsis? p)
                          (err "misplaced ellipsis in syntax-rules pattern"
                                pat))
                         (else #t))
                      (err "invalid syntax-rules pattern" pat)))))))

       (make-ids
        (lambda (keys pat err acc cntl ids)
           (syntax-case (pat pat)
              ((#t #t)
               (ellipsis? (cadr pat))
               (let ((x (generate-identifier 'x)))
                  (make-ids keys (car pat) err x (make-id x acc cntl) ids)))
              ((#t . #t)
               (make-ids keys
                         (car pat)
                         err
                         (quasisyntax (car ,acc))
                         cntl
                         (make-ids keys
                                   (cdr pat)
                                   err
                                   (quasisyntax (cdr ,acc))
                                   cntl
                                   ids)))
              (else
               (if (and (identifier? pat) (not (key? pat keys)))
                   (if (lookup pat ids)
                       (err "duplicate syntax-rules pattern variable" pat)
                       (cons (make-id pat acc cntl) ids))
                   ids)))))
  
       (gen-output
        (lambda (pat ids err)
           (let gen ((p pat) (loops '()))
              (if (identifier? p)
                  (let ((id (lookup p ids)))
                     (cond
                        (id
                         (add-control! pat (id-control id) loops err)
                         (list (syntax unquote) (id-access id)))
                        ((ellipsis? p)
                         (err "misplaced ellipsis in syntax-rules pattern" pat))
                        ((or (free-identifier=? p (syntax unquote))
                             (free-identifier=? p (syntax unquote-splicing))
                             (free-identifier=? p (syntax quasiquote))
                             (free-identifier=? p (syntax syntax)))
                         (quasisyntax (,(syntax unquote) (syntax ,p))))
                        (else p)))
                  (syntax-case (p p)
                     ((#t #t . #t)
                      (ellipsis? (cadr p))
                      (cons (list (syntax unquote-splicing)
                                  (let ((l (start-loop)))
                                     (gen-loop pat
                                               l
                                               (gen (car p)
                                                    (cons l loops))
                                               err)))
                            (gen (cddr p) loops)))
                     ((#t . #t)
                      (cons (gen (car p) loops) (gen (cdr p) loops)))
                     (else p))))))

       (lookup
        (lambda (i ids)
           (and (not (null? ids))
                (if (bound-identifier=? i (id-pattern (car ids)))
                    (car ids)
                    (lookup i (cdr ids))))))
  
       (add-control!
        (lambda (pat control loops err)
           (cond
              ((null? control))
              ((null? loops)
               (err "missing ellipsis in syntax-rules output pattern" pat))
              (else
               (let ((ids (loop-controls (car loops))))
                  (if (not (memq control ids))
                      (set-loop-controls! (car loops) (cons control ids))))
               (add-control! pat (id-control control) (cdr loops) err)))))

       (gen-loop
        (lambda (pat loop body err)
           (let ((ids (loop-controls loop)))
              (if (null? ids)
                  (err "extra ellipsis in syntax-rules output pattern" pat)
                  (syntax-case (body body)
                     ((unquote #t)
                      (let ((i (cadr body)))
                         (and (identifier? i)
                              (bound-identifier=? (id-pattern (car ids)) i)))
                      (id-access (car ids)))
                     ((unquote (#t #t))
                      (and (null? (cdr ids))
                           (let ((i (cadadr body)))
                              (and (identifier? i)
                                   (bound-identifier=? (id-pattern (car ids))
                                                       i))))
                      (quasisyntax (map ,(caadr body) ,(id-access (car ids)))))
                     (else
                      (quasisyntax (map (lambda ,(map id-pattern ids)
                                      ,(gen-syntax body))
                                   ,@(map id-access ids)))))))))

       (gen-syntax
        (lambda (x)
           (list (syntax quasisyntax) x)))

       (gen-clauses
        (lambda (source keys clauses err)
           (if (null? clauses)
               (quasisyntax (syntax-error "invalid syntax" ,source))
               (let ((input (caar clauses))
                     (output (cadar clauses))
                     (rest (cdr clauses)))
                  (quasisyntax
                     (syntax-dispatch
                        ,source
                        (syntax ,(gen-input keys input err))
                        (lambda (x)
                           ,(gen-syntax
                               (gen-output output
                                           (make-ids keys
                                                     input
                                                     err
                                                     (syntax x)
                                                     '()
                                                     '())
                                           err)))
                        (lambda (x) 
                          ,(gen-clauses (syntax x) keys rest err)))))))))

    (lambda (x)
       (syntax-case (x x)
          ((syntax-rules (#t ...) (#t #t) ...)
           (key-check (cadr x))
           (call-with-current-continuation
              (lambda (k)
                 (quasisyntax
                    (lambda (exp)
                       ,(gen-clauses (syntax exp)
                                     (cadr x)
                                     (cddr x)
                                     (lambda (m x)
                                        (k (syntax-error m x)))))))))
          (else (syntax-error "invalid syntax" x))))))
