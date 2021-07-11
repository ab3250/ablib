  
  (define (mode lst)
    (check-list lst "lst" "(mode lst)")
    (let* ([val-count (count-unique lst)]
	   [mx (apply max (map cdr val-count))])
      (filter (lambda (x) (not (null? x)))
	      (map (lambda (val count) (if (= count mx) val '()))
		   (map car val-count)
		   (map cdr val-count)))))

  (define (unique lst)
    (check-list lst "lst" "(unique lst)")
    (map car (count-unique lst)))

  (define (rle lst)
    (check-list lst "lst" "(rle lst)")
    (let loop ([first (car lst)]
               [rest (cdr lst)]
               [n 1]
               [vals '()]
               [counts '()])
      (cond
       [(null? rest)
	(map cons
	     (reverse (cons first vals))
	     (reverse (cons n counts)))]
       [(= first (car rest))
	(loop (car rest) (cdr rest) (add1 n) vals counts)]
       [else
	(loop (car rest) (cdr rest) 1 (cons first vals) (cons n counts))])))
  


    (define (percentile lst percent)
      (cond ((or (<= percent 0)
                 (>= percent 100))
             (error "Percentile: percent must be from 1 to 99, inclusive"))
            ((null? lst)
             (error "Percentile: List must not be null"))
            (else
              (let* ((sorted-vec (apply vector (list-sort < lst)))
                     (n (vector-length sorted-vec))
                     (k (* n (/ percent 100)))
                     (floor-k (floor k)))
                (if (= k floor-k)
                  (/ (+ (vector-ref sorted-vec k)
                        (vector-ref sorted-vec (- k 1)))
                     2)
                  (vector-ref sorted-vec floor-k))))))

   (define (median lst)
      (percentile lst 50))

   (define (standard-error-of-the-mean lst)
      (/ (standard-deviation lst)
         (sqrt (length lst))))

    ;; Number of ways to take k things from n without replacement, when order does not matter
    (define (combinations n k)
      (do ((i 0 (+ 1 i))
           (res 1 (/ (* res (- n i))
                     (- k i))))
        ((= i k) res)))

    ;; Number of ways to take k things from n without replacement, when order matters
 ;   (define (permutations n k)
 ;     (do ((i 0 (+ 1 i))
 ;          (res 1 (* res (- n i))))
 ;       ((= i k) res)))


  (define (mean lst)
    (check-list lst "lst" "(mean lst)")
    (/ (apply + lst) (length lst)))

  (define (variance lst)
    ;; ms is a pair of m and s variables
    ;; x is current value of lst in loop
    (define (update-ms x ms i) 
      (let* ([m (car ms)]
	     [s (cdr ms)]
	     [new-m (+ m (/ (- x m) (add1 i)))])
	(cons new-m
	      (+ s (* (- x m) (- x new-m))))))
    (check-list lst "lst" "(variance lst)")
    (let loop ([lst (cdr lst)]
	       [ms (cons (car lst) 0)] 
	       [i 1])                 ; one-based indexing in the algorithm
      (if (null? lst)
	  (/ (cdr ms) (- i 1))
          (loop (cdr lst) (update-ms (car lst) ms i) (add1 i)))))
  
  (define (standard-deviation lst)
    (check-list lst "lst" "(standard-deviation lst)")
    (sqrt (variance lst)))


  (define (sum-squares lst)
    (apply + (map (lambda (x) (expt x 2)) lst)))

;;;;;;

(define symbol=? eq?)



(define (andmap f xs)
    (cond ((null? xs) #t)
          ((f (car xs))
            (andmap f (cdr xs)))
          (else #f)))

(define (add1 x)(+ x 1))
(define (sub1 x)(- x 1))

(define assertion-violation
  (lambda args
    (display 'assertion-violation)
    (newline)
    (for-each pretty-print args)
    (newline)
    (error)))

  (define (count-unique lst)
    (check-list lst "lst" "(count-unique lst)")
    (rle (sort < lst)))
  

 (define (check-list lst lst-name who)
    (unless (list? lst)
      (assertion-violation who (string-append lst-name " is not a list")))
    (unless (for-all real? lst)
      (assertion-violation who (string-append "at least one element of " lst-name " is not a real number")))
    (when (null? lst)
      (assertion-violation who (string-append lst-name " is empty"))))

(define for-all andmap)

(define pretty-print   write)

(define (list-sort compare lst)
  ;;
  (define (merge left right)
    (let loop ((left left)
               (right right)
               (result '()))
      (cond
       ((and (null? left) (null? right))
        (reverse result))
       ((null? left)
        (loop left (cdr right) (cons (car right) result)))
       ((null? right)
        (loop (cdr left) right (cons (car left) result)))
       (else
        (let ((left1 (car left))
              (right1 (car right)))
          (if (compare left1 right1)
              (loop (cdr left) right (cons left1 result))
              (loop left (cdr right) (cons right1 result))))))))
  ;;
  (define (pair-map! proc lst)
    ;; { a, b, c, d[, e] } -> f -> { f(a, b), f(c, d)[, e] }
    (unless (or (null? lst) (null? (cdr lst)))
      (let ((first (car lst))
            (second (car (cdr lst)))
            (rest (cdr (cdr lst))))
        (set-car! lst (proc first second))
        (set-cdr! lst rest)
        (pair-map! proc rest))))
  ;;
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let ((sublists (map list lst)))
        (do ()
            ((null? (cdr sublists))
             (car sublists))
          (pair-map! merge sublists)))))

;  (define (sign x)
;    (cond ((negative? x) -1)
;          ((positive? x) 1)
;          ((zero? x) 0)
;          (else (error 'sign "requires a real number" x))))


  (define (diff lst)
    (check-list lst "lst" "(diff lst)")
    (let loop ([lst lst]
               [out '()])
      (cond [(null? (cdr lst))
             (reverse out)]
            [(loop (cdr lst)
                   (cons (- (cadr lst) (car lst)) out))])))

   (define (sign x)
    (cond [(< x 0) -1]
          [(> x 0) 1]
          [else 0]))

  (define (factorial n)
    (if (<= n 1)
      1
      (do ((i 1 (+ i 1))
           (res 1 (* res i)))
          ((> i n) res))))

#|

(set! *random-state* (random-state-from-platform))

  ;; random-pick:
  ;; random selection from list
  (define (random-pick items)
    (if (and (list? items) (not (null? items)))
      (list-ref items (random (length items)))
      #f))

  ;; random-sample:
  ;; Return a random sample of size N from sequence, without replacement.  
  ;; If N is equal to or greater than the length of the sequence, return 
  ;; the entire sequence.
  (define (random-sample n items)
    (cond ((<= n 0) 
           '())
          ((>= n (length items)) 
           items)
          (else
            (let loop ((remaining items)
                       (kept '()))
              (if (= (length kept) n)
                kept
                (let ((one (random-pick remaining)))
                  (loop (delete one remaining)
                        (cons one kept))))))))
|#
