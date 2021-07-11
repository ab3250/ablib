;; Selection sort.
;; Copyright Juha Heinanen 1988
;; This code may be freely distributed.
;;

(define (selection-sort! vector less?)
  (define (vector-swap! vector index1 index2)
    (let ((temp (vector-ref vector index1)))
      (vector-set! vector index1 (vector-ref vector index2))
      (vector-set! vector index2 temp)))
  ; selection-sort!
  (let ((last-index (- (vector-length vector) 1)))
    (let repeat1 ((position 0))
      (if (< position last-index)
          (let repeat2 ((smallest position) (index (+ position 1)))
            (if (<= index last-index)
                (if (less? (vector-ref vector index)
                           (vector-ref vector smallest))
                    (repeat2 index (+ index 1))
                    (repeat2 smallest (+ index 1)))
                (begin
                  (vector-swap! vector position smallest)
                  (repeat1 (+ position 1)))))))))
