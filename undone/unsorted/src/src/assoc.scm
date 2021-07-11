; assoc that accepts a comparison predicate

(define (assoc key records equal?) 
  (cond ((null? records) #f) 
        ((equal? key (caar records))
         (car records)) 
        (else
         (assoc key
               (cdr records)
               equal?)))) 
