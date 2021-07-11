;;;;
;;;; Brute force string searching.
;;;; The program really searches a sublist from a list of
;;;; characters, but the algorithm is exactly the same.
;;;;
;;;; Copyright Pertti Kellom\"aki, pk@cs.tut.fi, 1992
;;;; This file may be freely distributed.
;;;; 


(define (brutesearch pattern string)

  (define (matches? pattern string)
    (let loop ((pattern pattern)
               (string string))
      (cond ((null? pattern) #t)
            ((null? string) #f)
            (else
             (and (equal? (car pattern)
                          (car string))
                  (matches? (cdr pattern)
                            (cdr string)))))))

  ;; try each prefix of the string
  (let loop ((string string)
             (position 0))
    (cond ((null? string)
           #f)
          ((matches? pattern string)
           position)
          (else
           (loop (cdr string)
                 (+ position 1))))))
