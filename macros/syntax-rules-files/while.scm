;;;;While
(define-syntax while
  (syntax-rules ()
    ((while condition body ...)
     (let loop ()
       (if condition
           (begin
             body ...
             (loop))
           #f)))))

;;while example
(let ((i 0))
  (while (< i 10)
    (display i)
    (display #\x0020)
    (set! i (+ i 1))))
;;0 1 2 3 4 5 6 7 8 9
;;Unspecified return value
