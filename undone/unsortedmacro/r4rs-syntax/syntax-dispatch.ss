;;; syntax-dispatch.ss
;;; Robert Hieb & Kent Dybvig
;;; 90/10/07

; "syntax-dispatch" expects a syntax expression, a pattern (as
; described below), a success continuation and a fail continuation.  If
; the expression matches the pattern the success continuation is called
; with an expression unwrapped down to the level indicated by the
; pattern.  Otherwise, the fail continuation is called with the original
; expression.  It checks against patterns of the form:
;   #t                      : matches anything
;   <identifier>            : matches an identifier with "free-identifier=?"
;   (<pattern> ...)         : matches a proper list of <pattern>s 
;   (<pattern> . <pattern>) : matches car and cdr
;   ()                      : matches empty list
;   #(<pattern>*)           : matches vector

(define syntax-dispatch
   (lambda (expression pattern success fail)
      (let dispatch ((e expression) (p (unwrap-syntax pattern)) (k success))
         (cond
            ((eq? p #t)
             (k e))
            ((identifier? p)
             (if (and (identifier? e) (free-identifier=? e p))
                 (k e)
                 (fail expression)))
            ((pair? p)
             (let ((patcar (unwrap-syntax (car p)))
                   (patcdr (unwrap-syntax (cdr p))))
                (if (and (pair? patcdr)
                         (identifier? (car patcdr))
                         (free-identifier=? (car patcdr) (syntax ...))
                         (null? (unwrap-syntax (cdr patcdr))))
                    (let dispatch-list ((e (unwrap-syntax e)) (k k))
                       (cond
                          ((pair? e)
                           (dispatch (car e) patcar
                              (lambda (a)
                                 (dispatch-list (unwrap-syntax (cdr e))
                                    (lambda (d) (k (cons a d)))))))
                          ((null? e) (k '()))
                          (else (fail expression))))
                    (let ((e (unwrap-syntax e)))
                       (if (pair? e)
                           (dispatch (car e) patcar
                              (lambda (a)
                                 (dispatch (cdr e) patcdr
                                    (lambda (d) (k (cons a d))))))
                           (fail expression))))))
            ((null? p)
             (if (null? (unwrap-syntax e)) (k '()) (fail expression)))
            ((vector? p)
             (let ((e (unwrap-syntax e)))
                (if (vector? e)
                    (dispatch (vector->list e)
                              (vector->list p)
                              (lambda (x) (k (list->vector x))))
                    (fail expression))))
            (else (equal? p (unwrap-syntax e)))))))
