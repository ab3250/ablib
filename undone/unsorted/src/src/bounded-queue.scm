;; A queue implemented using a vector.
;; Copyright Juha Heinanen 1988
;; This code may be freely distributed.
;;

(define (make-bounded-queue queue-size)

  ; memory representation
  (define queue-space (+ queue-size 1))
  (define front-index 0)
  (define rear-index 0)
  (define queue (make-vector queue-space))

  (define (empty?)
    (= rear-index front-index))

  (define (full?)
    (= front-index
       (modulo (+ rear-index 1) queue-space)))

  (define (length)
    (if (>= rear-index front-index)
	(- rear-index front-index)
	(- queue-space (- front-index rear-index))))

  (define (front)
    (if (empty?)
        (error "Queue is empty -- FRONT" queue)
        (vector-ref queue (modulo (+ front-index 1)
				  queue-space))))

  (define (insert! object)
      (if (full?)
        (error "Queue overflow -- INSERT!" queue)
	(begin
	  (set! rear-index (modulo (+ rear-index 1)
				   queue-space))
	  (vector-set! queue rear-index object)
	  object)))

  (define (remove!)
    (if (empty?)
        (error "Queue underflow -- REMOVE!" queue)
        (let ((object (front)))
          (set! front-index (modulo (+ front-index 1)
				    queue-space))
          object)))

  (define (dispatch op . args)
    (case op
      ((length) (apply length args))
      ((front) (apply front args))
      ((insert!) (apply insert! args))
      ((remove!) (apply remove! args))
      (else (error "Unknown queue operation -- DISPATCH" op))))

  dispatch)
