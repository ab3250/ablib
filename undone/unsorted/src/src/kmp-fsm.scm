;;;;
;;;; Implementation of Knuth-Morris-Pratt algorithm for string searching.
;;;; See for example Sedgewick, Algorithms in C, Addison-Wesley 1990.
;;;;
;;;; Copyright Pertti Kellom\"aki, pk@cs.tut.fi, 1992
;;;; This file may be freely distributed.
;;;;

;;;
;;; (next partial-pattern)
;;; Given that the partial pattern has matched, how many positions do
;;; we need to back up from the current position to continue the search?
;;; 

(define (next partial-pattern)

  ;; (prefix? p1 p2)
  ;; Is p2 a prefix of p1?
  (define (prefix? p1 p2)
    (let loop ((p1 p1)
	       (p2 p2))
      (cond ((null? p2)
	     #t)
	    ((equal? (car p1)
		     (car p2))
	     (loop (cdr p1)
		   (cdr p2)))
	    (else
	     #f))))

  ;; Start sliding the partial pattern to the right.
  ;; Reverse the lists to make life easier.
  (let loop ((original (reverse partial-pattern))
	     (trial (cdr (reverse partial-pattern)))
	     (backup (- (length partial-pattern) 1)))
    (cond ((prefix? original trial)
	   backup)
	  (else
	   (loop original
		 (cdr trial)
		 (- backup 1))))))


;;;
;;; (backup-states pattern)
;;; Build a list of the consecutive states where the automaton should
;;; back when the 1st, 2nd, 3rd, etc. character did not match.

(define (backup-states pattern)
  (let loop ((pattern (reverse (cdr (reverse pattern))))
	     (result '()))
    (if (null? pattern)
	result
	(loop (reverse (cdr (reverse pattern)))   ; OK, it is inefficient...
	      (cons (next pattern)
		    result)))))

;;;
;;; (fsm-description pattern)
;;; Build a description of a (kmp) finite state machine for a given pattern.

(define (fsm-description pattern)
  
  (define (make-state-name num)
    (string->symbol
      (string-append "s"
                     (number->string num))))
  
  (define build-state list)
  (define build-transition list)
  
  (let loop ((description '())
             (backup (cons 0 (backup-states pattern)))
             (pattern pattern)
             (num 0))
    (if (null? pattern)
        (reverse (cons (build-state (make-state-name num)) description))
        (loop (cons (build-state
                      (make-state-name num)
                      (build-transition 
                        (car pattern)
                        (make-state-name (+ num 1))
                        (if (null? (cdr pattern))
			    (lambda x 'accept)
			    (lambda x 'consumed)))
                      (build-transition
                        'others
                        (make-state-name (car backup))
                        (if (> num 0) 
			    (lambda x 'fail)
			    (lambda x 'consumed))))
                    description)
              (cdr backup)
              (cdr pattern)
              (+ num 1)))))
          

;;;
;;; (kmp-fsm pattern)
;;; Return a finite state machine that accepts the pattern.
;;; The fsm will return the symbol 'consumed' for all transitions
;;; that have consumed a symbol, 'fail' for all failing transitions
;;; (meaning that the next input symbol should be fed again to the fsm),
;;; and 'accept' for the final transition that completes the match.

(define (kmp-fsm pattern)
  (make-fsm (fsm-description pattern)))


;;;
;;; Now test this little toy: the procedure kmp takes a pattern,
;;; builds a fsm and starts driving it. If debug? is non-nil, 
;;; the state of the machine will be echoed before each transition.

(define (kmp pattern . debug?)

  (define (debug fsm)
    (if debug?
	(begin (display (fsm 'fsm-state))
	       (newline))))
  
  (let ((fsm (kmp-fsm pattern)))
    (debug fsm)
    (let loop ((sym (read)))
      (let ((res (fsm sym)))
	(display res)(newline)
          (case res
          ((accept)
	   (debug fsm)
	   'accept)
          ((consumed)
	   (debug fsm)
           (loop (read)))
          ((fail)
	   (debug fsm)
           (loop sym)))))))
