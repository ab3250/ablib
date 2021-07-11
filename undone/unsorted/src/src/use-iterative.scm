;;;;
;;;; Using the iterative improvement methods
;;;; 

;;;
;;; Some good-enough criteria
;;;

(define abs-good-enough?
  (make-absolute-good-enough 0.001))

(define rel-good-enough?
  (make-relative-good-enough 0.001))

;;;
;;; Some functions
;;; 

(define (square x) (* x x))

(define (x2+3x-4 x)
  (+ (square x)
     (* 3 x)
     -4))

(define (sincos x)
  (sin (cos x)))


(newton x2+3x-4 0 0.001 abs-good-enough?)
(newton sincos 0 0.001 abs-good-enough?)
(secant x2+3x-4 0 1 abs-good-enough?)
(secant sincos 0 1 abs-good-enough?)

(newton x2+3x-4 0 0.001 rel-good-enough?)
(newton sincos 0 0.001 rel-good-enough?)
(secant x2+3x-4 0 1 rel-good-enough?)
(secant sincos 0 1 rel-good-enough?)

;;;
;;; Computing square root with newton
;;;

(define (my-sqrt x)
  (newton (lambda (y) (- (square y) x))
	  1
	  0.001
	  rel-good-enough?))

(my-sqrt 2)
(my-sqrt 100)
(my-sqrt 10000)
