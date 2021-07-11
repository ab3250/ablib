(define-syntax-rule (swap! x y)
  (let ((tmp x))
    (set! x y)
    (set! y tmp)))
