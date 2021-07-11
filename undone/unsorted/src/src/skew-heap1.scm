;; Skew heap.
;; Copyright Juha Heinanen 1988
;; This code may be freely distributed.
;;

(define (make-skew-heap lt? value-list)	; explicit vector version
  
  (define heap (heapify value-list))

  (define (heapify value-list)
    ; heapifies value-list by a two-way merge
    (define (merge heap-list new-heap-list)
      (if (null? heap-list)
          new-heap-list
          (if (null? (cdr heap-list))
              (cons (car heap-list) new-heap-list)
              (merge (cddr heap-list)
                     (cons (meld! (car heap-list) (cadr heap-list))
                           new-heap-list)))))
    (if (null? value-list)
        '()
        (let loop
          ((heap-list
            (merge
             (map (lambda (x) (vector '() x '())) value-list)
             '())))
          (if (null? (cdr heap-list))
              (car heap-list)
              (loop (merge heap-list '()))))))
  
  (define (left h) (vector-ref h 0))
  (define (info h) (vector-ref h 1))
  (define (right h) (vector-ref h 2))
  
  (define (set-left! h v) (vector-set! h 0 v))
  (define (set-right! h v) (vector-set! h 2 v))
  
  (define (insert! x)
    (set! heap (meld! (heapify (list x)) heap)))
  
  (define (find-min)
    (if (null? heap)
        '()
        (info heap)))
  
  (define (delete-min!)
    (if (null? heap)
        '()
        (let ((result (info heap)))
          (set! heap (meld! (left heap) (right heap)))
          result)))
  
  (define (meld! h1 h2)
    (define (meld1! h1 h2 parent)
      (if (null? h1)
          (set-left! parent h2)
          (if (lt? (info h1) (info h2))
              (let ((t (right h1)))
                (set-left! parent h1)
                (set-right! h1 (left h1))
                (meld1! t h2 h1))
              (let ((t (right h2)))
                (set-left! parent h2)
                (set-right! h2 (left h2))
                (meld1! t h1 h2)))))
    (if (null? h2)
        h1
        (let ((result (vector '() '() '())))
          (meld1! h1 h2 result)
          (left result))))
  
  (define (select op . args)
    (case op
      (insert! (apply insert! args))
      (find-min (find-min))
      (delete-min! (delete-min!))
      (else (error "Unknown operation MAKE-SKEW-HEAP" op))))
  select)

(define (heap-sort1 list)
  (define heap (make-skew-heap < '()))
  (for-each (lambda (x) (heap 'insert! x)) list)
  (let loop ((result '()) (next (heap 'delete-min!)))
    (if (null? next)
        result
        (loop (cons next result) (heap 'delete-min!)))))
