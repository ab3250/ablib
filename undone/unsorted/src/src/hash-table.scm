;; A hash table implementation.
;; Copyright Juha Heinanen 1988
;; This code may be freely distributed.
;;

(define (make-hash-table size ord equal?)

  ; memory representation
  (define hash-table (make-vector size '()))
  (define item-count 0)

  ; utility procedure
  (define (hash item) (modulo (ord item) size))

  (define (lookup item)
    (let search ((item-list (vector-ref hash-table (hash item))))
      (if (null? item-list)
          #f
          (if (equal? item (car item-list))
              (car item-list)
              (search (cdr item-list))))))

  (define (insert! item)
    (let* ((index (hash item)) (item-list (vector-ref hash-table index)))
      (let search ((current item-list))
        (if (null? current)
            (begin
              (vector-set! hash-table index (cons item item-list))
              (set! item-count (+ item-count 1)))
            (if (equal? item (car current))
                (set-car! current item)
                (search (cdr current)))))))

  (define (delete! item)
    (let* ((index (hash item)) (item-list (vector-ref hash-table index)))
      (let search ((current item-list) (previous '()))
        (if (not (null? current))
            (if (equal? item (car current))
                (begin
                  (if (null? previous)
                      (vector-set! hash-table index (cdr item-list))
                      (set-cdr! previous (cdr current)))
                  (set! item-count (- item-count 1)))
                (search (cdr current) current))
            (error "Item not found in hash table -- DELETE!" item)))))

  (define (count) item-count)

  (define (dispatch op . args)
    (case op
      ((insert!) (apply insert! args))
      ((lookup) (apply lookup args))
      ((delete!) (apply delete! args))
      ((count) (apply count args))
      (else (error "Unknown hash table operation - DISPATCH" op))))

  dispatch)
