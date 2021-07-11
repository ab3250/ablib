;; Merge sort.
;; Copyright Juha Heinanen 1988
;; This code may be freely distributed.
;;

(define (merge-sort value-list less?)
  ; Sorts value-list according to less?
  (define (merge-pass! list-of-lists previous)
    ; Two way merges lists in list-of-lists into cdr of previous
    (define (merge! list1 list2 previous)
      ; Merges list1 and list2 into cdr of previous
      (if (null? list1)
          (set-cdr! previous list2)
          (if (null? list2)
              (set-cdr! previous list1)
              (if (less? (car list2) (car list1))
                  (begin
                   (set-cdr! previous list2)
                   (merge! list1 (cdr list2) list2))
                  (begin
                   (set-cdr! previous list1)
                   (merge! (cdr list1) list2 list1))))))
    ; merge-pass!
    (if (not (null? list-of-lists))
        (if (null? (cdr list-of-lists))
            (set-cdr! previous list-of-lists)
            (let ((sorted-list (list '*header-node*)))
              (merge! (car list-of-lists) (cadr list-of-lists) sorted-list)
              (set-cdr! previous (list (cdr sorted-list)))
              (merge-pass! (cddr list-of-lists) (cdr previous))))))
    ; merge-sort!
    (let ((merged-lists (list '*header-node* '())))
      (let loop ((list-of-lists (map list value-list)))
        (merge-pass! list-of-lists merged-lists)
        (if (null? (cddr merged-lists))
            (cadr merged-lists)
            (loop (cdr merged-lists))))))

