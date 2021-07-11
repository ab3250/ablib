;;; Several of the test programs that are included within
;;; a SRFI sample implementation appear to use
;;; "the Chicken test egg, which is provided on Chibi as
;;; the (chibi test) library."  So we have to fake that here.
;;;
;;; The Chicken test egg appears to be documented at
;;; http://wiki.call-cc.org/eggref/4/test
;;;
;;; For an example of a test program that includes this file,
;;; see srfi-134-test.sps7

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This stuff was copied from test/R7RS/Lib/tests/scheme/test.sld
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Good enough for this file.

(define (for-all f xs . others)
  (cond ((null? xs)
	 #t)
	((apply f (car xs) (map car others))
	 (apply for-all f (cdr xs) (map cdr others)))
	(else
	 #f)))

(define (exists f xs . others)
  (cond ((null? xs)
	 #f)
	((apply f (car xs) (map car others))
	 #t)
	(else
	 (apply exists f (cdr xs) (map cdr others)))))

(define (get-string-n p n)
  (let loop ((chars '())
	     (i 0))
    (if (= i n)
	(list->string (reverse chars))
	(let ((c (read-char p)))
	  (if (char? c)
	      (loop (cons c chars)
		    (+ i 1))
	      (loop chars n))))))

(define-record-type err
  (make-err err-c)
  err?
  (err-c err-err-c))

(define-record-type expected-exception
  (make-expected-exception)
  expected-exception?)

(define-record-type multiple-results
  (make-multiple-results values)
  multiple-results?
  (values multiple-results-values))

(define-record-type approx
  (make-approx value)
  approx?
  (value approx-value))

(define-record-type alts (make-alts values) alts?
  (values alts-values))

(define-syntax larceny:test    ; FIXME: renamed
  (syntax-rules ()
    ((_ expr expected)
     (begin
       ;; (write 'expr) (newline)
       (run-test 'expr
		 (catch-exns (lambda () expr))
		 expected)))))

(define (catch-exns thunk)
  (guard (c (#t (make-err c)))
	 (call-with-values thunk
	   (lambda x
	     (if (= 1 (length x))
		 (car x)
		 (make-multiple-results x))))))

(define-syntax test/approx
  (syntax-rules ()
    ((_ expr expected)
     (run-test 'expr
	       (make-approx expr)
	       (make-approx expected)))))

(define-syntax test/alts
  (syntax-rules ()
    ((_ expr expected0 expected ...)
     (run-test 'expr
	       expr
	       (make-alts (list expected0 expected ...))))))

(define (good-enough? x y)
  ;; relative error should be with 0.1%, but greater
  ;; relative error is allowed when the expected value
  ;; is near zero.
  (cond ((not (number? x)) #f)
	((not (number? y)) #f)
	((or (not (real? x))
	     (not (real? y)))
	 (and (good-enough? (real-part x) (real-part y))
	      (good-enough? (imag-part x) (imag-part y))))
	((infinite? x)
	 (= x (* 2.0 y)))
	((infinite? y)
	 (= (* 2.0 x) y))
	((nan? y)
	 (nan? x))
	((> (magnitude y) 1e-6)
	 (< (/ (magnitude (- x y))
	       (magnitude y))
	    1e-3))
	(else
	 (< (magnitude (- x y)) 1e-6))))

;; FIXME

(define-syntax test/exn
  (syntax-rules ()
    ((_ expr condition)
     (test (guard (c (((condition-predicate
			(record-type-descriptor condition)) c)
		      (make-expected-exception)))
		  expr)
	   (make-expected-exception)))))

(define-syntax test/values
  (syntax-rules ()
    ((_ expr val ...)
     (run-test 'expr
	       (catch-exns (lambda () expr))
	       (make-multiple-results (list val ...))))))

(define-syntax test/output
  (syntax-rules ()
    ((_ expr expected str)
     (run-test 'expr
	       (capture-output
		(lambda ()
		  (run-test 'expr
			    (guard (c (#t (make-err c)))
				   expr)
			    expected)))
	       str))))

(define-syntax test/unspec
  (syntax-rules ()
    ((_ expr)
     (test (begin expr 'unspec) 'unspec))))

;; FIXME

(define-syntax test/unspec-or-exn
  (syntax-rules ()
    ((_ expr condition)
     (test (guard (c (((condition-predicate
			(record-type-descriptor condition)) c)
		      'unspec))
		  (begin expr 'unspec))
	   'unspec))))

;; FIXME

(define-syntax test/unspec-flonum-or-exn
  (syntax-rules ()
    ((_ expr condition)
     (test (guard (c (((condition-predicate
			(record-type-descriptor condition)) c)
		      'unspec-or-flonum))
		  (let ((v expr))
		    (if (flonum? v)
			'unspec-or-flonum
			(if (eq? v 'unspec-or-flonum)
			    (list v)
			    v))))
	   'unspec-or-flonum))))

(define-syntax test/output/unspec
  (syntax-rules ()
    ((_ expr str)
     (test/output (begin expr 'unspec) 'unspec str))))

(define checked 0)
(define failures '())

(define (capture-output thunk)
  (if (file-exists? "tmp-catch-out")
      (delete-file "tmp-catch-out"))
  (dynamic-wind
      (lambda () 'nothing)
      (lambda ()
        (with-output-to-file "tmp-catch-out"
	  thunk)
        (call-with-input-file "tmp-catch-out"
	  (lambda (p)
	    (get-string-n p 1024))))
      (lambda ()
        (if (file-exists? "tmp-catch-out")
            (delete-file "tmp-catch-out")))))

(define (same-result? got expected)
  (cond
   ((and (real? expected) (nan? expected))
    (and (real? got) (nan? got)))
   ((expected-exception? expected)
    (expected-exception? got))
   ((approx? expected)
    (and (approx? got)
	 (good-enough? (approx-value expected)
		       (approx-value got))))
   ((multiple-results? expected)
    (and (multiple-results? got)
	 (= (length (multiple-results-values expected))
	    (length (multiple-results-values got)))
	 (for-all same-result?
		  (multiple-results-values expected)
		  (multiple-results-values got))))
   ((alts? expected)
    (exists (lambda (e) (same-result? got e))
	    (alts-values expected)))
   (else
    ;(equal? got expected))))
    ((current-test-comparator)
     got expected))))

(define (run-test expr got expected)
  (set! checked (+ 1 checked))
  (unless (same-result? got expected)
	  (set! failures
		(cons (list expr got expected)
		      failures))))

(define (write-result prefix v)
  (cond
   ((multiple-results? v)
    (for-each (lambda (v)
		(write-result prefix v))
	      (multiple-results-values v)))
   ((approx? v)
    (display prefix)
    (display "approximately ")
    (write (approx-value v)))
   ((alts? v)
    (write-result (string-append prefix "   ")
		  (car (alts-values v)))
    (for-each (lambda (v)
		(write-result (string-append prefix "OR ")
			      v))
	      (cdr (alts-values v))))
   (else
    (display prefix)
    (write v))))

(define (report-test-results)
  (if (null? failures)
      (begin
	(display checked)
	(display " tests passed\n"))
      (begin
	(display (length failures))
	(display " tests failed:\n\n")
	(for-each (lambda (t)
		    (display "Expression:\n ")
		    (write (car t))
		    (display "\nResult:")
		    (write-result "\n " (cadr t))
		    (display "\nExpected:")
		    (write-result "\n " (caddr t))
		    (display "\n\n"))
		  (reverse failures))
	(display (length failures))
	(display " of ")
	(display checked)
	(display " tests failed.\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End of stuff copied from test/R7RS/Lib/tests/scheme/test.sld
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (iequal? x y)
  (cond ((and (ipair? x) (ipair? y))
         (and (iequal? (icar x) (icar y))
              (iequal? (icdr x) (icdr y))))
        ((and (pair? x) (pair? y))
         (and (iequal? (car x) (car y))
              (iequal? (cdr x) (cdr y))))
        ((and (vector? x)
              (vector? y))
         (iequal? (vector->list x) (vector->list y)))
        (else
         (equal? x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Definitions that fake part of the Chicken test egg.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax test-group
  (syntax-rules ()
   ((_ name expr)
    expr)
   ((_ name expr-or-defn stuff ...)
    (let ()
      expr-or-defn
      (test-group name stuff ...)))))

(define-syntax test
  (syntax-rules ()
   ((_ name expected actual)
    (begin
     ;; (write 'actual) (newline)
     (run-test '(begin name actual)
               (catch-exns (lambda () actual))
               expected)))
   ((_ expected actual)
    (test 'anonymous expected actual))))

(define-syntax test-assert
  (syntax-rules ()
   ((_ name expr)
    (parameterize ((current-test-comparator iequal?))
     (test name #t (and expr #t))))
   ((_ expr)
    (test-assert 'anonymous expr))))

(define-syntax test-deny
  (syntax-rules ()
   ((_ name expr)
    (parameterize ((current-test-comparator iequal?))
     (test name #t (and (not expr) #t))))
   ((_ expr)
    (test-deny 'anonymous expr))))

(define-syntax test-error
  (syntax-rules ()
   ((_ name expr)
    (test/exn expr &condition))
   ((_ expr)
    (test-error 'anonymous expr))))

(define-syntax test-end
  (syntax-rules ()
   ((_ name)
    (begin (report-test-results)
           (display "Done.")
           (newline)))
   ((_)
    (test-end 'anonymous))))

(define (test-exit . rest)
  (let ((error-status (if (null? rest) 1 (car rest))))
    (if (null? failures)
        (exit)
        (exit error-status))))        

(define current-test-comparator
  (make-parameter iequal?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End of definitions faking part of the Chicken test egg.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

