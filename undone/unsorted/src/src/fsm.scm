;;;;
;;;; Finite state machines in Scheme
;;;;
;;;; Copyright Pertti Kellom\"aki, pk@cs.tut.fi, 1992
;;;; This file may be freely distributed.
;;;;


;;;
;;; (make-fsm description)
;;;
;;; Returns a finite state machine constructed from the description.
;;; The description is of the form
;;; 
;;;    ((<state name> (<input symbol> <state name> <action>) ...) ...)
;;;    
;;; For example, a simple automaton that accepts the language
;;; "b", "ab", "aab", "aaab",... :
;;; 
;;;    ((start #f
;;;            ("a" start) 
;;;            ("b" end))
;;;     (end))
;;;
;;; The names of states can be any Scheme objects that can be compared with
;;; equal? The action is a procedure that will be called when the transition
;;; is performed. The procedure is called with the input symbol that caused 
;;; the transition and the name of the new state.
;;;
;;; The result of (make-fsm ...) is a procedure object (closure) that
;;; can be reset to the initial state by calling it with the symbol fsm-reset.
;;; The name of the current state can be obtained by calling the fsm with
;;; the symbol fsm-state. Any other argument is taken to be the next input 
;;; symbol. The automaton returns the value obtained by calling the action
;;; for the new state.
;;; 

(define (make-fsm description)

  ;; Put the states here
  (define states '())

  ;; State abstraction
  (define (make-state name) (list name))
  (define state-name car)
  (define state-transitions cdr)

  ;; Transition abstraction
  (define (make-transition symbol target action)
    (list symbol target action))
  (define transition-target cadr)
  (define transition-action caddr)

  ;; Transition description
  (define get-transition-name car)
  (define (get-transition-target trans)
    (get-state (cadr trans)))
  (define (get-transition-action trans)
    (if (cddr trans)
        (caddr trans)
        #f))

  ;; Add a new transition (a input state pair) to a state
  (define (add-transition! state input target action)
    (set-cdr! state
              (cons (make-transition input target action)
                    (cdr state))))

  ;; Iterate through the list of transitions for one state specified in
  ;; the description.
  (define (add-transitions! state-description)
    (let ((state (get-state (car state-description)))
          (transitions (cdr state-description)))

      (let loop ((transitions transitions))
        (if (null? transitions)
            #f
            (let ((this (car transitions)))
              (add-transition! state
                               (get-transition-name this)
                               (get-transition-target this)
                               (get-transition-action this))
              (loop (cdr transitions)))))))


  ;; Look up a state by name
  (define (get-state name)
    (let loop ((states states))
      (cond ((equal? name (state-name (car states)))
             (car states))
            ((null? states)
             (error "make-fsm: No such state: " name))
            (else
             (loop (cdr states))))))
  
  ;; Build a list of states, initially empty.
  ;; Do it in reverse order, so that the initial state will end up
  ;; being the first state.
  (let loop ((description (reverse description)))
    (cond ((null? description)
           #f)
          (else
           (set! states
                 (cons (make-state (caar description))
                       states))
           (loop (cdr description)))))


  ;; Add transitions to the states
  (let loop ((description description))
    (if (null? description)
        #f
        (begin
          (add-transitions! (car description))
          (loop (cdr description)))))

  ;; The automaton proper
  (let ((current-state (car states)))
    (lambda (sym)
      (cond ((equal? sym 'fsm-reset)
             (set! current-state (car states))
             #t)
            ((equal? sym 'fsm-state)
             (state-name current-state))
            (else
              (let ((transition 
                      (or (assoc sym (state-transitions current-state))
                          (assoc 'others (state-transitions current-state)))))
                (if transition
                    (begin
                      (set! current-state
                            (transition-target transition))
                      (if (transition-action transition)
                          ((transition-action transition)
                           sym 
                           (state-name current-state)))))))))))
                    

(define (fsm-trace symbol state) state)

(define s
  (make-fsm
    `((start (a mid ,fsm-trace)
             (b end ,fsm-trace)
             (c start ,fsm-trace))
      (mid (a end ,fsm-trace)
           (b end ,fsm-trace)
           (c start ,fsm-trace))
      (end (c start ,fsm-trace)))))

;;;
;;; A fsm that finds the first match of a*b+c+ and collects the result.
;;;

(define accumulator
  (let ((store '()))
    (lambda (m . args)
      (case m
        ((flush) (set! store '()))
        ((collect) (set! store (cons (car args) store)))
        ((result) (reverse store))))))

(define a*b+c+
  (let ((flush (lambda (sym state) (accumulator 'flush) #f))
        (collect (lambda (sym state) (accumulator 'collect sym) #f))
        (quit (lambda (sym state) (accumulator 'result))))

    (make-fsm
      `((start (a start ,collect)
               (b mid ,collect)
               (others start ,flush))
        (mid (b mid ,collect)
             (c end ,collect)
             (others start ,flush))
        (end (c end ,collect)
             (others start ,quit))))))
           

(define (test)
  (a*b+c+ 'fsm-reset)
  (let loop ((res (a*b+c+ (read))))
    (display (a*b+c+ 'fsm-state)) (newline)
    (if res
        res
        (loop (a*b+c+ (read))))))
