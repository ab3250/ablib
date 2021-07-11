;;;;;;For
(define-syntax for
  (syntax-rules ()
    ((_ (i from to) b1 ...)
     (let loop((i from))
       (when (< i to)
	 b1 ...
	 (loop (1+ i)))))))


(define for (lambda (start end func)
              (let loop ((index start))
                (if (> index end) #t
                    (begin (func index) (loop (+ index 1))) ))))

;;for example
(for (i 0 10)
  (display i)
  (display #\Space))
;;0 1 2 3 4 5 6 7 8 9
;;Unspecified return value
