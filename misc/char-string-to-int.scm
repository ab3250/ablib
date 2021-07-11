(define (numerical-char->integer char)
  (let ([num (- (char->integer char) 48)]) ; 48 = (char->integer #\0)
    (if
     (or (< num 0) (> num 9))
     (raise 'non-numerical-char #t)
     num)))

(define (string->integer str)
  (let ([char-list (string->list str)])
    (if (null? char-list)
        (raise 'empty-string #t)
        (foldl
         (λ([x : Integer] [y : Integer])
           (+ (* y 10) x))
         0
         (map numerical-char->integer char-list)))))

