;;;;
;;;; Evaluation of simple expressions of the form
;;;;
;;;; <expr> = <term> + <term> + ...$
;;;; <term> = <factor> * <factor> * ...
;;;; <factor> = integer
;;;;
;;;; All tokens must be whitespace separated. The input is terminated
;;;; with a $
;;;;
;;;; Copyright Pertti Kellom\"aki, pk@cs.tut.fi, 1992
;;;; This file may be freely distributed.
;;;;

(define (eval-expr)

  (define operands (make-stack "Operands: "))
  (define operators (make-stack "Operators: "))
  
  (define (perform-operation)
    (let ((operator (operators 'pop!)))
      (case operator
        ((+)
         (operands 'push!
                   (+ (operands 'pop!)
                      (operands 'pop!))))
        ((*)
         (operands 'push!
                   (* (operands 'pop!)
                      (operands 'pop!))))
        (else
         (error "Bad operator:" operator)))))

  (define get-token
    (let ((previous 'none))
      (lambda ()
        (cond ((eq? previous '$)
               previous)
              (else
               (set! previous (read))
               previous)))))


  (operators 'push! 'dummy)
  (let loop ((token (get-token)))
    (if (and (eq? (operators 'top) 'dummy)
             (eq? token '$))
        (operands 'pop!)
        (case token
          ((+ * $)
           (let ((pending (operators 'top)))
             (if (operator-> pending token)
                 (perform-operation)
                 #f)
             (if (not (eq? token '$))
                 (operators 'push! token)
                 #f)
             (loop (get-token))))
          (else
           (operands 'push! token)
           (loop (get-token)))))))

(define (operator-> op1 op2)
  (define (weight op)
    (case op
      ((dummy) 0)
      (($) 1)
      ((+) 2)
      ((*) 3)))

  (> (weight op1) (weight op2)))

;;;
;;; A stack implementation
;;;

(define (make-stack name) 

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
    (display name)
    (display op)
    (display " ")
    (display args)
    (newline)
    (case op 
      ((empty?) (apply empty? args)) 
      ((top) (apply top args)) 
      ((push!) (apply push! args)) 
      ((pop!) (apply pop! args)) 
      (else 
        (error "Unknown stack operation --"
          " DISPATCH" op)))) 
  dispatch) 


