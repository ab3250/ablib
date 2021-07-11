;; A queue implementation using lists.
;; Copyright Juha Heinanen 1988
;; This code may be freely distributed.
;;

(define (make-queue)

  ; header node abstraction
  (define (make-header-node) (cons '() '()))
  (define node-front car)
  (define node-rear cdr)
  (define node-set-front! set-car!)
  (define node-set-rear! set-cdr!)

  ; memory representation
  (define queue (make-header-node))
  (define queue-length 0)

  (define (length) queue-length)

  (define (front)
    (if (= queue-length 0)
        (error "Queue is empty -- FRONT" queue)
        (car (node-front queue))))

  (define (insert! object)
    (let ((new-pair (cons object '())))
      (if (= queue-length 0)
          (begin
            (node-set-front! queue new-pair)
            (node-set-rear! queue new-pair))
          (begin
            (set-cdr! (node-rear queue) new-pair)
            (node-set-rear! queue new-pair))))
    (set! queue-length (+ queue-length 1))
    object)

  (define (remove!)
    (if (= queue-length 0)
        (error "Queue underflow -- REMOVE!" queue)
        (let ((object (front)))
          (node-set-front! queue (cdr (node-front queue)))
          (set! queue-length (- queue-length 1))
          object)))

  (define (dispatch op . args)
    (case op
      ((length) (apply length args))
      ((front) (apply front args))
      ((insert!) (apply insert! args))
      ((remove!) (apply remove! args))
      (else (error "Unknown queue operation -- DISPATCH" op))))

  dispatch)
