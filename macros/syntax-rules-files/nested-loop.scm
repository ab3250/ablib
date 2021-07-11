
(define-syntax nested-loop
  (syntax-rules ()
    ((_ l1 l1-start l1-end l2 l2-start l2-end body ...)
         (for l1-start l1-end (lambda(l1)
		    (for l2-start l2-end (lambda(l2)
			       (begin
				 body ... ))))))))

(define for (lambda (start end func)
              (let loop ((index start))
                (if (> index end) #t
                    (begin (func index) (loop (+ index 1))) ))))

(nested-loop row 0 8 col 0 8
(display row)(display col)(newline)
)


; (define fill-array(lambda (start1 end1 start2 end2 arry)
;   (let loop1 ((index1 start1)(arry1 arry))
;     (if (< index1 end1)
;       (let loop2 ((index2 start2)(arry2 arry1))
;         (if(< index2 end2)
;           (loop2 (+ index2 1)(append arry2 (list(string-append (padn index1 2) (padn index2 2)))))
;           (loop1 (+ index1 1) arry2)))
;       arry1))))