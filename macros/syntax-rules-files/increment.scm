;;;;;
(define-syntax increment
(syntax-rules ()
((_ x) (begin (set! x (+ x 1)) x))
((_ x i) (begin (set! x (+ x i)) x))))

;;increment example
(let ((i 0) (j 0))
  (incf i)
  (incf j 3)
  (display (list 'i '= i))
  (newline)
;
(display (list 'j '= j)))
(i = 1)
(j = 3)
;;Unspecified return value
