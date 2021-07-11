;; An example course registration system using an ordered list and a queue
;; Copyright Juha Heinanen 1988
;; This code may be freely distributed.
;;

(define (make-course-register capacity)

  (define student-list (make-ordered-list student-equal? student-less?))
  (define waiting-list (make-queue))

  (define (register! student)
    (if (= (student-list 'length) capacity)
        (begin
          (waiting-list 'insert! student)
          (display-line "You were placed in the waiting list."))
        (if (not (student-list 'search student))
            (begin
              (student-list 'insert! student)
              (display-line
	       "You have successfully registered for the course."))
            (display-line
             "You have already registered for the course."))))

  (define (drop! student)
    (if (student-list 'search student)
        (begin
          (student-list 'remove! student)
          (display-line "You have successfully dropped the course.")
          (let loop ()
            (if (> (waiting-list 'length) 0)
		(let ((waiting-student (waiting-list 'remove!)))
		  (if (not (student-list 'search waiting-student))
		      (student-list 'insert! waiting-student)
		      (loop))))))
        (display-line "You have not registered for the course.")))

  (define (roster)
    (display-line "Course Roster:")
    (newline)
    (student-list
      'for-each
      (lambda (x) (student-display x) (newline)))
    (newline)
    (display "Waiting list: ")
    (display (waiting-list 'length))
    (display " ")
    (display-line "students"))

  (define (dispatch op . args)
    (case op
      ((register!) (apply register! args))
      ((drop!) (apply drop! args))
      ((roster) (apply roster args))
      (else (error "Unknown course register operation -- DISPATCH" op))))

  dispatch)

(define (make-student number first-name last-name)
  (list number first-name last-name))

(define student-number car)

(define student-first-name cadr)

(define student-last-name caddr)

(define (student-equal? s1 s2)
  (= (student-number s1) (student-number s2)))

(define (student-less? s1 s2)
  (< (student-number s1) (student-number s2)))

(define (student-display student)
  (display (student-number student))
  (display " ")
  (display (student-first-name student))
  (display " ")
  (display (student-last-name student)))

(define (student-enter)
  (display "Enter number: ")
  (let ((number (read)))
    (display "Enter first name: ")
    (let ((first-name (read)))
      (display "Enter last name: ")
      (let ((last-name (read)))
        (make-student number first-name last-name)))))

(define (course-registration-system)
  (define course-register (make-course-register 3))
  (let menu ()
    (newline)
    (display-line "Course registration system")
    (newline)
    (display-line "r - register")
    (display-line "d - drop")
    (display-line "p - print")
    (display-line "q - quit")
    (newline)
    (display "Enter your choise: ")
    (let ((choise (read)))
      (newline)
      (case choise
        ((r)
         (course-register 'register! (student-enter))
         (menu))
        ((d)
         (course-register 'drop! (student-enter))
         (menu))
        ((p)
         (course-register 'roster)
         (menu))
        ((q))
        (else (menu))))))
