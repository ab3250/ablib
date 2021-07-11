(define (string<? x y)
  (let ((xl (string-length x)) (yl (string-length y)))
    (let repeat ((i 0))
      (if (and (< i xl) (< i yl))
	  (let ((ch1 (string-ref x i)) (ch2 (string-ref y i)))
	    (if (char<? ch1 ch2)
		#t
		(if (char=? ch1 ch2)
		    (repeat (+ i 1))
		    #f)))
	  (< i yl)))))

(define (string>? x y)
  (let ((xl (string-length x)) (yl (string-length y)))
    (let repeat ((i 0))
      (if (and (< i xl) (< i yl))
	  (let ((ch1 (string-ref x i)) (ch2 (string-ref y i)))
	    (if (char>? ch1 ch2)
		#t
		(if (char=? ch1 ch2)
		    (repeat (+ i 1))
		    #f)))
	  (< i xl)))))

(define (string<=? x y)
  (let ((xl (string-length x)) (yl (string-length y)))
    (let repeat ((i 0))
      (if (and (< i xl) (< i yl))
	  (let ((ch1 (string-ref x i)) (ch2 (string-ref y i)))
	    (if (char<? ch1 ch2)
		#t
		(if (char=? ch1 ch2)
		    (repeat (+ i 1))
		    #f)))
	  (or (< i yl) (= i xl))))))

(define (string>=? x y)
  (let ((xl (string-length x)) (yl (string-length y)))
    (let repeat ((i 0))
      (if (and (< i xl) (< i yl))
	  (let ((ch1 (string-ref x i)) (ch2 (string-ref y i)))
	    (if (char>? ch1 ch2)
		#t
		(if (char=? ch1 ch2)
		    (repeat (+ i 1))
		    #f)))
	  (or (< i xl) (= i yl))))))

(define (with-input-from-file str thunk)
  (let ((saved-input-port current-input-port))
    (set! current-input-port (open-input-file str))
    (thunk)
    (close-input-port current-input-port)
    (set! current-input-port saved-input-port)))

(define (with-output-to-file str thunk)
  (let ((saved-output-port current-output-port))
    (set! current-input-port (open-output-file str))
    (thunk)
    (close-output-port current-output-port)
    (set! current-output-port saved-output-port)))

