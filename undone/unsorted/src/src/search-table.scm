;; Search table.
;; Copyright Juha Heinanen 1988
;; This code may be freely distributed.
;;

(load "assoc.scm")

(define (make-search-table equal?) 

  (define search-table (list '*search-table*)) 

  (define (insert! key value) 
    (let ((record (assoc key
                         (cdr search-table)
                         equal?))) 
      (if (null? record) 
          (set-cdr! search-table 
                    (cons (cons key value)
                          (cdr search-table))) 
          (set-cdr! record value))) 
    value) 

  (define (lookup key) 
    (let ((record (assoc key
                         (cdr search-table)
                         equal?))) 
      (if (null? record) 
          #f 
          (cdr record)))) 

  (define (delete! key)
    (let loop ((prev search-table)
               (next (cdr search-table)))
      (if (not (null? next))
          (if (equal? key (caar next))
              (set-cdr! prev (cdr next))
              (loop next (cdr next)))
          (error "Key is not in search table -- DELETE!"
                 key))))

  (define (dispatch op . args) 
    (case op 
      ((insert!) (apply insert! args)) 
      ((lookup) (apply lookup args)) 
      ((delete!) (apply delete! args)) 
      (else
       (error "Unknown operation -- SEARCH-TABLE"
              op)))) 

  dispatch) 
