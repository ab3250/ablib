(define-library (print)
(import (scheme base)
(scheme write))
(export print
println
prints
printsln)
(begin
;; Print arguments.
(define (print . args)
(for-each display args))
;; Print arguments, space separated.
(define (prints . args)
(for-each (lambda (s)
(display s)
(display " ")) args))
;; Print arguments and a newline.
(define (println . args)
(apply print args)
(newline))
;; Print arguments, space separated, newlin
(define (printsln . args)
(apply prints args)
(newline))))
