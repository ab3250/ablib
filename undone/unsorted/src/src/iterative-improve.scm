;;;;
;;;; Using higher-order procedures to implement two iterative
;;;; methods.
;;;;
;;;; Copyright Pertti Kellomaki, pk@cs.tut.fi, 1992
;;;; This file may be freely distributed.
;;;;


;;;
;;; iterative-improve: iterate until the approximate value produced
;;;   is good enough

(define (iterative-improve f guess previous-guess good-enough? improve)
  (if (good-enough? guess previous-guess f)
      guess
      (iterative-improve
       f
       (improve guess previous-guess)
       guess
       good-enough?
       improve)))

;;;
;;; Two test for testing if the guess if good enough
;;;


(define (make-absolute-good-enough epsilon)
  (lambda (guess previous f)
    (< (abs (f guess))
       epsilon)))

(define (make-relative-good-enough epsilon)
  (lambda (guess previous f)
    (< (/ (abs (- guess previous))
	  guess)
       epsilon)))

;;;
;;; Newton's method
;;;

(define (deriv f dx)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x))
       dx)))

(define (make-newton-improve f dx)
  (let ((df (deriv f dx)))
    (lambda (x prev-x)
      (- x (/ (f x)
	      (df x))))))

(define (newton f guess dx good-enough?)
  (let ((improve (make-newton-improve f dx)))
    (iterative-improve f guess (improve guess 0)
		       good-enough?
		       improve)))

    
;;;
;;; Secant method
;;;

(define (make-secant-improve f)
  (lambda (xk xk-1)
    (- xk (* (f xk)
	    (/ (- xk xk-1)
	       (- (f xk)
		  (f xk-1)))))))

(define (secant f guess1 guess2 good-enough?)
  (iterative-improve f guess1 guess2
		     good-enough?
		     (make-secant-improve f)))

