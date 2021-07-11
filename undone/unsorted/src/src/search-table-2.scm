;; Two-dimensional search tables.
;; Copyright Juha Heinanen 1988
;; This code may be freely distributed.
;;

(load "search-table.scm")

(define (make-search-table-2 equal?) 

  (define search-table (make-search-table equal?))

  (define (insert! key-1 key-2 value) 
    (let ((subtable (search-table 'lookup key-1)))
      (if (null? subtable)
          (let ((subtable (make-search-table equal?)))
            (subtable 'insert! key-2 value)
            (search-table 'insert! key-1 subtable))
          (subtable 'insert! key-2 value)))
     value)

  (define (lookup key-1 key-2) 
    (let ((subtable (search-table 'lookup key-1)))
      (if (null? subtable)
          #f
          (subtable 'lookup key-2))))

  (define (delete! key-1 key-2)
    (let ((subtable (search-table 'lookup key-1)))
      (if (null? subtable)
          (error "Key is not in search table -- DELETE!"
                 key-1)
          (subtable 'delete! key-2))))

  (define (dispatch op . args) 
    (case op 
      ((insert!) (apply insert! args)) 
      ((lookup) (apply lookup args)) 
      ((delete!) (apply delete! args)) 
      (else 
       (error "Unknown operation -- SEARCH-TABLE-2"
              op)))) 

  dispatch) 
