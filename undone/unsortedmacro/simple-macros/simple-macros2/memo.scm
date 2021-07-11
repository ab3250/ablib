
; Memoization

; See file memoize.txt to find out why the obvious
;
;  (define (memoize! old-exp new-exp)
;    (set-car! old-exp (car new-exp))
;    (set-cdr! old-exp (cdr new-exp))))
;
; loses.

(define (memoize! old-exp new-exp)
  (table-set! *memo-table* old-exp new-exp))

(define (memo-perhaps exp)
  (or (table-ref *memo-table* exp)
      exp))

(define *memo-table* #f)

(define (flush-memo-table!)
  (set! *memo-table* (make-table sxhash)))

; The use of this hash function assumes that the object will not be
; mutated.

; The depth limit is an attempt to ensure constant run time.

(define (sxhash obj)
  (abs (let recur ((obj obj) (n *sxhash-depth*))
	 (cond ((symbol? obj) (string-hash (symbol->string obj)))
	       ((string? obj) (string-hash obj))
	       ((integer? obj)
		(if (exact? obj) obj (inexact->exact obj)))
	       ((char? obj) (char->integer obj))
	       ((eq? obj #f) 1001)
	       ((eq? obj #t) 1002)
	       ((null? obj) 1003)
	       ((pair? obj)
		(if (= n 0)
		    1004
		    (+ (recur (car obj) (- n 1))
		       (recur (cdr obj) (- n 1)))))
	       ((vector? obj)
		(if (= n 0)
		    1005
		    (do ((i (min *sxhash-depth* (- (vector-length obj) 1))
			    (- i 1))
			 (h 1006 (+ h (recur (vector-ref obj i) (- n 1)))))
			((< i 0) h))))
	       ((rational? obj)
		(let ((obj (if (exact? obj) obj (inexact->exact obj))))
		  (+ (numerator obj) (denominator obj))))
	       (else 1006)))))

(define (string-hash s)
  (do ((i (min (- (string-length s) 1) 400) (- i 1))
       (h (string-length s) (+ h (char->integer (string-ref s i)))))
      ((< i 0) h)))

(define *sxhash-depth* 4)
