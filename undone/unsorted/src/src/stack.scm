;;;;
;;;; A stack implementation.
;;;;

(define (make-stack) 
  (define stack '()) 
  (define (empty?) (null? stack)) 

  (define (top) 
    (if (null? stack) 
        (error "Stack is empty -- TOP"
               stack) 
        (car stack))) 

  (define (push! object) 
    (set! stack (cons object stack)) 
    object) 

  (define (pop!) 
    (if (null? stack) 
        (error "Stack underflow -- POP!"
               stack) 
        (let ((object (car stack))) 
          (set! stack (cdr stack)) 
          object))) 

  (define (dispatch op . args) 
    (case op 
      ((empty?) (apply empty? args)) 
      ((top) (apply top args)) 
      ((push!) (apply push! args)) 
      ((pop!) (apply pop! args)) 
      (else 
        (error "Unknown stack operation --"
          " DISPATCH" op)))) 
  dispatch) 
