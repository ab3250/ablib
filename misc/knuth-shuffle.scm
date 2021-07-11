(define (knuth-shuffle lst-org)  
  (let loop ((count (length lst-org)) (lst lst-org))      
    (if (zero? count)
    	lst
	(let*   ((j (random count))
		 (new-count (- count 1))
	         (tmp (list-ref lst new-count))
	         (lst2 (list-set lst new-count (list-ref lst j)))
	         (lst3 (list-set lst2 j tmp)))	         
	         (loop new-count lst3)))))





