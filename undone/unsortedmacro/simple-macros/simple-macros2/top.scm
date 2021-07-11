
; Test routine for evaluator

(define (tst e)
  (flush-memo-table!)
  (ev e top-level-env))


; Test routine for expander

(define (ex exp)
  (expand exp top-level-env))

