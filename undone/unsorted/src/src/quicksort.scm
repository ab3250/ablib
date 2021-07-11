;; Quicksort.
;; Copyright Juha Heinanen 1988
;; This code may be freely distributed.
;; Warning! This code uses a multiple value return facility, which is
;; a feature of some particular implementation (MIT Scheme, I think).
;;

(define (vector-swap! vec i1 i2)
  ; swaps vector elements in indices i1 and i2
  (let ((temp (vector-ref vec i1)))
    (vector-set! vec i1 (vector-ref vec i2))
    (vector-set! vec i2 temp)))

(define (quicksort! vec less?)
  ; Sorts vector vec according to less?
  (define (sort first last)
    ; sorts vector elements vec[first] ... vec[last]
    (define pivot
      (vector-ref vec (quotient (+ first last) 2)))
    (define (partition left right)
      ; partitions vec[first] .. vec[last] so that
      ; (a) vec[k] <= pivot, when
      ;     k = first .. left-index - 1
      ; (b) vec[k] >= pivot, when
      ;     k = right-index + 1 .. last
      ; (c) vec[k] = pivot, when
      ;     k = right-index + 1 ... left-index - 1
      (define left-index
        (let repeat ((index left))
          (if (less? (vector-ref vec index) pivot)
              (repeat (+ index 1))
              index)))
      (define right-index
        (let repeat ((index right))
          (if (less? pivot (vector-ref vec index))
              (repeat (- index 1))
              index)))
      ; partition
      (cond
       ((< left-index right-index)
	(vector-swap! vec left-index right-index)
	(partition (+ left-index 1) (- right-index 1)))
       ((= left-index right-index)
	(return (+ left-index 1) (- right-index 1)))
       (else
	(return left-index right-index))))
    ; sort
    (let ((left-index right-index (partition first last)))
      (if (< first right-index)
	  (sort first right-index))
      (if (< left-index last)
	  (sort left-index last))))
  ; quicksort
  (let ((high (- (vector-length vec) 1)))
    (if (>= high 0) (sort 0 high))))
