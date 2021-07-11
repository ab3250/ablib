;; Binary search tree.
;; Copyright Juha Heinanen 1988
;; This code may be freely distributed.
;;

(define (make-search-tree equal? less?)

  ; memory representation
  (define tree '())
  (define item-count 0)
  
  ; node abstraction
  (define (make-node left item right)
    (list item left right))
  (define node-left cadr)
  (define node-item car)
  (define node-right caddr)
  (define (node-set-left! n v)
    (set-car! (cdr n) v))
  (define node-set-item! set-car!)
  (define (node-set-right! n v)
    (set-car! (cddr n) v))
  
  (define (insert! object)
    (let insert ((node tree) (parent '()))
      (if (not (null? node))
          (if (equal? object (node-item node))
              (node-set-item! node object)
              (if (less? object (node-item node))
                  (insert (node-left node) node)
                  (insert (node-right node) node)))
          (let ((new-node (make-node '() object '())))
            (set! item-count (+ item-count 1))
            (if (null? parent)  ; Is node the root?
                (set! tree new-node)
                (if (less? object (node-item parent))
                    (node-set-left! parent new-node)
                    (node-set-right! parent new-node)))))))
  
  (define (lookup object)
    (let search ((node tree))
      (if (not (null? node))
          (if (equal? object (node-item node))
              (node-item node)
              (if (less? object (node-item node))
                  (search (node-left node))
                  (search (node-right node))))
          #f)))
  
  (define (delete! object)
    (define (replace! node parent replacement)
      ; Replaces node with replacement;
      (if (null? parent)       ; Is node the root?
          (set! tree replacement)
          (if (eq? node (node-left parent))
              (node-set-left! parent replacement)
              (node-set-right! parent replacement))))
    (define (replace-smallest! node smallest previous)
      ; Finds the inorder successor of node, stores its item to node,
      ; and deletes it from previous
      (if (null? (node-left smallest))
          (begin  ; smallest was found
            (node-set-item! node (node-item smallest))
            (if (eq? node previous)  ; smallest = (node-right node)?
                (node-set-right! node (node-right smallest))
                (node-set-left! previous (node-right smallest))))
          (replace-smallest! node (node-left smallest) smallest)))
    ; delete!
    (let delete ((node tree) (parent '()))
      (if (not (null? node))
          (if (equal? object (node-item node))
              (begin
                (set! item-count (- item-count 1))
                (if (null? (node-left node))
                    (replace! node parent (node-right node))
                    (if (null? (node-right node))
                        (replace! node parent (node-left node))
                        ; node has two children
                        (replace-smallest! node (node-right node) node))))
              (if (less? object (node-item node))
                  (delete (node-left node) node)
                  (delete (node-right node) node)))
          (error "Object is not in search tree -- DELETE!" object))))

  (define (count) item-count)
  
  (define (for-each proc)
    (let repeat ((node tree))
      (if (not (null? node))
          (begin
            (repeat (node-left node))
            (proc (node-item node))
            (repeat (node-right node))))))
  
  (define (dispatch op . args)
    (case op
      ((lookup) (apply lookup args))
      ((insert!) (apply insert! args))
      ((delete!) (apply delete! args))
      ((count) (apply count args))
      ((for-each) (apply for-each args))
      (else (error "Unknown search tree operation -- DISPATCH" op))))
  
  dispatch)
