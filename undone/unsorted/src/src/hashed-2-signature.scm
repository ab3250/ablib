;;;;
;;;; String searching using static 2-signatures.
;;;; See for example Azmoodeh: Abstract Data Types and Algorithms, 2nd
;;;; edition, MacMillan 1990.
;;;;
;;;; Copyright Pertti Kellom\"aki, pk@cs.tut.fi, 1992
;;;; This file may be freely distributed.
;;;;

(define bit-vector-size 32)

(define (hash c1 c2)
  (modulo (+ (char->integer c1) (char->integer c2))
	  bit-vector-size))

;;;
;;; Compute the signature of a line (string)
;;; Spaces are ignored, because they cause heavy bias in the hash
;;; function. 

(define (signature line)
  (let loop ((chars (string->list line))
	     (sig (make-vector bit-vector-size 0)))
    (cond ((< (length chars) 2)
	   sig)
	  ((char=? #\space (car chars))
	   (loop (cdr chars)
		      sig))
	  ((char=? #\space (cadr chars))
	   (loop (cons (car chars)
		       (cddr chars))
		 sig))
	  (else
	   (vector-set! sig
			(hash (car chars)
			      (cadr chars))
			1)
	   (loop (cdr chars)
		 sig)))))


;;;
;;; (pre-test? searched-sig candidate-sig)
;;; Compare the signatures to determine if it is possible for
;;; searched-sig to be included in candidate-sig.
;;;

(define (pre-test? searched-sig candidate-sig)

  (define (log-and s1 s2)
    (let loop ((result (make-vector bit-vector-size))
	       (index 0))
      (cond ((= index bit-vector-size)
	     result)
	    (else
	     (vector-set! result
			  index
			  (if (and (= (vector-ref s1 index) 1)
				   (= (vector-ref s2 index) 1))
			      1
			      0))
	     (loop result
		   (+ index 1))))))

  (equal? (log-and searched-sig candidate-sig)
	  searched-sig))

;;;
;;; (pre-test-file pattern file-name)
;;; Return a list of line numbers that pass the pre test in file
;;; called file-name.
;;;

(define (pre-test-file pattern file-name)

  (define (read-line f)
    (let loop ((c (read-char f))
	       (line '()))
      (cond ((and (eof-object? c)
		  (null? line))
	     c)
	    ((eof-object? c)
	     (reverse line))
	    ((char=? c #\newline)
	     (reverse line))
	    (else
	     (loop (read-char f)
		   (cons c line))))))

  (let ((file (open-input-file file-name))
	(search-sig (signature pattern)))
    (let loop ((line (read-line file))
	       (line-number 1)
	       (matching-lines '()))
      (cond ((eof-object? line)
	     (close-input-port file)
	     (reverse matching-lines))
	    ((pre-test? search-sig (signature (list->string line)))
	     (loop (read-line file)
		   (+ line-number 1)
		   (cons line-number matching-lines)))
	    (else
	     (loop (read-line file)
		   (+ line-number 1)
		   matching-lines))))))

;;;;
;;;; (pre-test-file "definition"
;;;;                "/usr/local/lib/emacs/texinfo/calc-2.01.texinfo")
;;;;
;;;; passed 6520 lines out of 34134. That is, 81% of the lines were
;;;; discarded based on their signature.
;;;;
;;;; There were 134 lines where the word "definition" occured, so the
;;;; hit rate was 2%
;;;; 