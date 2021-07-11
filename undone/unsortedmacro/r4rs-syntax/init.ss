;;; init.ss
;;; Robert Hieb & Kent Dybvig
;;; 90/10/07
;;; modified 91/01/02

; These initializations are done here rather than "expand.ss" so that
; "expand.ss" can be loaded twice (for bootstrapping purposes).


(define global-environment
   (let ([p '()])
      (lambda args
         (if (not (null? args))
             (set! p (car args))
             p))))

(define expand #f)
(define unwrap-syntax #f)
(define identifier? #f)
(define free-identifier=? #f)
(define bound-identifier=? #f)
(define identifier->symbol #f)
(define generate-identifier #f)
(define syntax-error #f)
(define construct-identifier #f)

;;; bootstrap exports

; These can be removed after the system rebuilds itself.

(define boot-install-global-transformer #f)
(define boot-make-global-identifier #f)
