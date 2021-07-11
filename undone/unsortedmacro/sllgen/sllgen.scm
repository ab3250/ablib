;;; sllgen -- Scheme LL(1) parser generator

;;; for COM 3351 class

;;; ****************************************************************

;;; Mon Oct 25 09:47:14 1993

;;; Fixed sllgen:make-parse-table-production-entry to check whether
;;; (first-of-list rhs) contains (), rather than just checking (null?
;;; rhs).  [bug fix]

;;; Mon Sep 27 12:51:00 1993:

;;; all.scm :  single file with entire package
;;;            procedures renamed to avoid name conflicts

;;; started Tue Aug 17 09:52:13 1993

;;; ****************************************************************        

;;; Table of contents:

;;; top.s                     top-level entries
;;; syntax.s                  concrete syntax for grammars, etc.
;;; eliminate-arbno.s         replaces (ARBNO lhs) items with new productions
;;; first-and-follow.s        calculate first and follow sets
;;; gen-table.s               take list of productions, first and
;;;                           follow tables, and generate parsing table
;;; check-table.s             take a parse table and check for conflicts
;;; scan.s                    scanner using streams
;;; parse.s                   run the generated parser
;;; tests.s                   test languages and examples

;;; ****************************************************************

;;; top.s

;;; Steps in parser generation:

;;; 1.  Eliminate arbno's by making new nonterms with goto's and
;;; emit-list's. 

;;; 2.  Factor productions with common prefixes (not in this version).

;;; 3.  Compute first and follow sets

;;; 4.  Compute prediction table & generate actions

;;; ****************************************************************

;;; top-level entries

(define sllgen:grammar->parser
  (lambda (grammar)
    (let* ((g (sllgen:eliminate-arbnos-from-productions grammar))
           (first-table (sllgen:first-table g))
           (follow-table (sllgen:follow-table
                           (sllgen:grammar->start-symbol grammar)
                           g
                           first-table))
           (table
             (sllgen:productions->parsing-table g first-table follow-table)))
      (sllgen:check-table table)
      table)))

(define sllgen:string->tree
  (lambda (scanner grammar)
    (let ((parser (sllgen:grammar->parser grammar)))
      (lambda (string)
        (sllgen:parse-top-level parser (sllgen:grammar->start-symbol
                                         grammar) 
          (sllgen:string->token-stream scanner string))))))


;;; ****************************************************************

;;; syntax.s :  concrete syntax for grammars, etc.

;;; ****************************************************************

;;; Concrete Syntax for grammars

;;; <Grammar> ::= (<production> ...)  ;; nonterm of first prod is
;;;                                      start symbol.
;;; <production> ::= (lhs rhs action)
;;;
;;; lhs ::= symbol                    ;; a symbol that appears in a
;;;                                      lhs is a non-term, all others
;;;                                      are terminals
;;; rhs ::= (rhs-item ...)
;;;
;;; rhs-item ::= symbol | (ARBNO . rhs)
;;;
;;; action ::= symbol | EMIT-LIST | GOTO

;;; ****************************************************************

;;; Auxiliaries for dealing with syntax of grammars

(define sllgen:grammar->productions 
  (lambda (gram) gram))                 ; nothing else now, but this
                                        ; might change

(define sllgen:grammar->start-symbol
  (lambda (gram)
    (sllgen:production->lhs
      (car 
        (sllgen:grammar->productions gram)))))

(define sllgen:make-production
  (lambda (lhs rhs action)
    (list lhs rhs action)))

(define sllgen:production->lhs car)
(define sllgen:production->rhs cadr)
(define sllgen:production->action caddr)

(define sllgen:productions->non-terminals
  (lambda (productions)
    (map sllgen:production->lhs productions)))

(define sllgen:arbno?
  (lambda (rhs-item)
    (and (pair? rhs-item)
         (eq? (car rhs-item) 'arbno))))

(define sllgen:arbno->rhs cdr)

(define sllgen:goto-action
  (lambda () 'goto))

(define sllgen:emit-list-action
  (lambda () 'emit-list))

;;; ****************************************************************

;;; updatable associative tables

;;; table ::= ((symbol . list) ...)


(define sllgen:make-initial-table       ; makes table with all entries
                                        ; initialized to empty
  (lambda (symbols)
    (map list symbols)))

(define sllgen:add-value-to-table!
  (lambda (table key value)
    (let ((pair (assq key table)))
      (if (memq value (cdr pair))
        #f
        (begin
          (set-cdr! pair (cons value (cdr pair)))
          #t)))))

(define sllgen:table-lookup
  (lambda (table key)
    (cdr (assq key table))))

(define sllgen:uniq
  (lambda (l)
    (if (null? l) '()
      (let ((z (sllgen:uniq (cdr l))))
        (if (memq (car l) z)
          z
          (cons (car l) z))))))

(define sllgen:union
  (lambda (s1 s2)                       ; s1 and s2 already unique
    (if (null? s1) s2
      (if (memq (car s1) s2)
        (sllgen:union (cdr s1) s2)
        (cons (car s1) (sllgen:union (cdr s1) s2))))))
      
(define sllgen:rember
  (lambda (a s)
    (cond 
      ((null? s) s)
      ((eqv? a (car s)) (cdr s))
      (else (cons (car s) (sllgen:rember a (cdr s)))))))


;;; ****************************************************************

;;; eliminate-arbno.s

;;; replaces (ARBNO lhs) items with new productions

(define sllgen:eliminate-arbnos-from-rhs
  (lambda (rhs k)
    ;; returns to its continuation the new rhs and the list of
    ;; new productions
    (cond
      ((null? rhs)
       (k rhs '()))
      ((sllgen:arbno? (car rhs))
       (let ((new-nonterm (gensym)))
         (sllgen:eliminate-arbnos-from-rhs
           (cdr rhs)
           (lambda (new-rhs new-prods)
             (k
               (cons new-nonterm new-rhs)
               (cons
                 (sllgen:make-production
                   new-nonterm '() (sllgen:emit-list-action))
                 (cons
                   (sllgen:make-production
                     new-nonterm
                     (append (sllgen:arbno->rhs (car rhs))
                           (list new-nonterm))
                     (sllgen:goto-action))
                   new-prods)))))))
      (else
        (sllgen:eliminate-arbnos-from-rhs (cdr rhs)
          (lambda (new-rhs new-prods)
            (k (cons (car rhs) new-rhs)
               new-prods)))))))

(define sllgen:eliminate-arbnos-from-production
  (lambda (production)
    ;; returns list of productions
    (sllgen:eliminate-arbnos-from-rhs
      (sllgen:production->rhs production)
      (lambda (new-rhs new-prods)
        (let ((new-production
                (sllgen:make-production 
                  (sllgen:production->lhs production)
                  new-rhs
                  (sllgen:production->action production))))
          (cons new-production
                (sllgen:eliminate-arbnos-from-productions new-prods)))))))
      
(define sllgen:eliminate-arbnos-from-productions
  (lambda (productions)
    (if (null? productions)
      '()
      (append
        (sllgen:eliminate-arbnos-from-production (car productions))
        (sllgen:eliminate-arbnos-from-productions (cdr productions))))))
              
;;; ****************************************************************

;;; first-and-follow.s

;;; calculate first and follow sets

;;; base conditions:

;;; A -> a ...   => a in first(A)
;;; A -> ()      => nil in first(A)

;;; closure conditions:

;;; A -> (B1 ... Bk c ...) & nil in first(B1)...first(Bk) => c in first(A)
;;; A -> (B1 ... Bk C ...) & nil in first(B1)...first(Bk) & c in first(C) =>
;;;                                                         c in first(A) 
;;; A -> (B1 ... Bk) & nil in first(B1)...first(Bk) => nil in first(A)

(define sllgen:first-table
  (lambda (productions)
    (let* ((non-terminals
             (sllgen:uniq (map sllgen:production->lhs productions)))
           (table (sllgen:make-initial-table non-terminals)))
      (letrec
        ((loop 
           ;; initialize with the base conditions and return the
           ;; productions to be considered for the closure
           (lambda (productions)
             (cond
               ((null? productions) '())
               ((null? (sllgen:production->rhs (car productions)))
                ;; A -> ()      => nil in first(A)
                (sllgen:add-value-to-table! table 
                  (sllgen:production->lhs (car productions))
                  '())
                (loop (cdr productions)))
               ((memq (car
                        (sllgen:production->rhs
                          (car productions)))
                       non-terminals)
                ;; this one is for the closure
                (cons (car productions)
                      (loop (cdr productions))))
               (else
                 ;; this one must start with a terminal symbol
                 (sllgen:add-value-to-table! table
                     (sllgen:production->lhs (car productions))
                     (car
                       (sllgen:production->rhs
                         (car productions)))))))))
        (let ((closure-productions (loop productions)))
          (sllgen:iterate-over-first-table table productions
            non-terminals)))))) 


(define sllgen:iterate-over-first-table
  (lambda (table productions non-terminals)
    (let* ((changed? '**uninitialized**)
           (add-value!
             (lambda (key value)
               (let ((not-there?
                       (sllgen:add-value-to-table! table key value)))
                 (set! changed? (or changed? not-there?)))))
           (first (lambda (key) (sllgen:table-lookup table key))))
      (letrec
        ((rhs-loop
           (lambda (lhs rhs)
             ;; assume everything in the rhs up to this point has () in
             ;; its first set
             (cond
               ((null? rhs)
                ;; A -> (B1 ... Bk) & nil in first(B1)...first(Bk) =>
                ;; nil in first(A) 
                (add-value! lhs '()))
               ;; A -> (B1 ... Bk C ...) & nil in first(B1)...first(Bk)
               ((memq (car rhs) non-terminals)
                (for-each 
                  (lambda (sym)
                    (if (not (null? sym))
                      ;; & c in first(C) => c in first(A) 
                      (add-value! lhs sym)
                      ;; e in first(C) -- continue to search down rhs
                      (rhs-loop lhs (cdr rhs))))
                  (first (car rhs))))
               (else
                 ;; A -> (B1 ... Bk c ...) & nil in
                 ;; first(B1)...first(Bk) => c in first(A) 
                 (add-value! lhs (car rhs))))))
         (main-loop
           (lambda ()
             (set! changed? #f)
             (for-each
               (lambda (production)
                 (rhs-loop 
                   (sllgen:production->lhs production)
                   (sllgen:production->rhs production)))
               productions)
             (if changed?
               (main-loop)
               table))))
         (main-loop)))))

(define sllgen:first-of-list
  (lambda (first-table non-terminals items)
    (letrec
      ((loop (lambda (items)
               (cond
                 ((null? items) '(()))             ; ans = {e}
                 ((memq (car items) non-terminals)
                  (let ((these
                          (sllgen:table-lookup first-table (car items))))
                    (if (memq '() these)
                      (let ((others (loop (cdr items))))
                        (let inner ((these these))
                          (cond
                            ((null? these) others)
                            ((null? (car these))
                             (inner (cdr these)))
                            ((memq (car these) others)
                             (inner (cdr these)))
                            (else
                              (cons (car these)
                                    (inner (cdr these)))))))
                      these)))
                 (else (list (car items)))))))
      (loop items))))

(define sllgen:follow-table
  (lambda (start-symbol productions first-table)
    (let* ((non-terminals
             (sllgen:uniq (map sllgen:production->lhs productions)))
           (table (sllgen:make-initial-table non-terminals))
           (changed? '**uninitialized**)
           (sllgen:add-value!
             (lambda (key value)
               (let ((not-there?
                       (sllgen:add-value-to-table! table key value)))
                 (set! changed? (or changed? not-there?)))))
           ;; closure-rules ::= ((a b) ...) means follow(a) \subset
           ;; follow(b)
           (closure-rules '()))
      (sllgen:add-value! start-symbol 'end-marker)
      (letrec
        ((init-loop
           ;; loops through productions once, adding starting values
           ;; to follow-table and other productions to closure-rules
           (lambda (productions)
             (if (null? productions)
               #t
               (let ((production (car productions)))
                 (rhs-loop
                   (sllgen:production->lhs production)
                   (sllgen:production->rhs production))
                 (init-loop (cdr productions))))))
         (rhs-loop
           (lambda (lhs rhs)
             (cond
               ((null? rhs) #t)
               ((memq (car rhs) non-terminals)
                ;; we've found a nonterminal.  What's it followed by?
                (let* ((rest (cdr rhs))
                       (first-of-rest
                         (sllgen:first-of-list
                           first-table non-terminals rest)))
                  (for-each 
                    (lambda (sym)
                      (if (not (null? sym))
                        ;; A -> (... B C ...) => first(C...) \subset follow(B)
                        (sllgen:add-value! (car rhs) sym)
                        ;; A -> (... B C ...) & e \in first(C ...) =>
                        ;; follow(A) \subset follow (B)
                        (set! closure-rules (cons (list lhs (car rhs))
                                                  closure-rules))))
                    first-of-rest))
                ;; now keep looking
                (rhs-loop lhs (cdr rhs)))
               (else
                 ;; this one's not a non-terminal.  Keep looking.
                 (rhs-loop lhs (cdr rhs))))))
         (closure-loop
           (lambda ()
             (set! changed? #f)
             (for-each
               (lambda (rule)
                 (let ((a (car rule))
                       (b (cadr rule)))
                   ;; follow(a) \subset follow(b)
                   (for-each
                     (lambda (sym)
                       (sllgen:add-value! b sym))
                     (sllgen:table-lookup table a))))
               closure-rules)
             (if changed?
               (closure-loop)
               table))))
        (init-loop productions)
        (closure-loop)))))
             

;;; ****************************************************************

;;; gen-table.s

;;; gen-table.s  take list of productions, first and follow tables,
;;; and generate parsing table

;;; table ::= ((non-terminal (list-of-items action ...)....) ...)

;;; the list of items is the first(rhs) for each production (or
;;; follow(lhs) if the production is empty.  We should probably check
;;; to see that these are non-intersecting, but we probably won't on
;;; this pass.

;;; First thing to do: collect all the productions for a given
;;; non-terminal.  This gives data structure of the form

;;; ((lhs production ...) ...)

;;; We'll do this using updatable tables.

(define sllgen:group-productions
  (lambda (productions)
    (let* ((non-terminals
             (sllgen:uniq (map sllgen:production->lhs productions)))
           (table (sllgen:make-initial-table non-terminals)))
      (for-each
        (lambda (production)
          (let 
            ((lhs (sllgen:production->lhs production)))
            (sllgen:add-value-to-table! table lhs production)))
        productions)
      table)))

;; this one uses the list structure of tables.  [Watch out]

(define sllgen:productions->parsing-table
  (lambda (productions first-table follow-table)
    (let ((non-terminals
            (sllgen:uniq (map sllgen:production->lhs productions)))
          (table (sllgen:group-productions productions)))
      (map 
        (lambda (table-entry)
          (sllgen:make-parse-table-non-terminal-entry
            (car table-entry)
            (map
              (lambda (production)
                (sllgen:make-parse-table-production-entry
                  production non-terminals first-table follow-table))
              (cdr table-entry))))
        table))))

(define sllgen:make-parse-table-non-terminal-entry
  (lambda (lhs entries)
    (cons lhs entries)))

(define sllgen:make-parse-table-production-entry
  (lambda (production non-terminals first-table follow-table)
    (let* ((rhs (sllgen:production->rhs production))
           (first-of-rhs (sllgen:first-of-list
                           first-table non-terminals
                           (sllgen:production->rhs production)))
           (steering-items
             (if (memq '() first-of-rhs)
               (sllgen:union
                 (sllgen:table-lookup
                   follow-table
                   (sllgen:production->lhs production))
                 (sllgen:rember '() first-of-rhs))
               first-of-rhs)))
      (cons steering-items
            (sllgen:make-parse-table-rhs-entry
              non-terminals
              (sllgen:production->rhs production)
              (sllgen:production->action production))))))

(define sllgen:make-parse-table-rhs-entry
  (lambda (non-terminals rhs action)
    (let loop ((rhs rhs))
      (cond
        ((null? rhs) 
         ;; at end -- emit reduce action or emit-list action
         (if (eq? action (sllgen:emit-list-action))
           (list (list action))
           (list (list 'reduce action))))
        ((and
           (null? (cdr rhs))
           (eq? action (sllgen:goto-action)))
         ;; if action is goto, emit (goto non-term) as last action
         (list (list (sllgen:goto-action) (car rhs))))
        ((memq (car rhs) non-terminals)
         (cons (list 'non-term (car rhs))
               (loop (cdr rhs))))
        (else
          (cons (list 'term (car rhs))
                (loop (cdr rhs))))))))

;;; ****************************************************************
           
;;; check-table.s

;;; take a parse table and check for conflicts

;;; table ::= ((non-terminal (list-of-items action ...)....) ...)

(define sllgen:check-table
  (lambda (table)
    (for-each sllgen:check-productions table)))

(define sllgen:check-productions
  (lambda (non-terminal-entry)
    (let ((non-terminal (car non-terminal-entry))
          (productions (cdr non-terminal-entry)))
      ;; see if the list-of-items are pairwise disjoint
      (let loop ((productions productions))
        (if (null? productions)
          #t                            ; no more to check
          (let ((this-production (car productions))
                (other-productions (cdr productions)))
            ;; check this production
            (for-each
              (lambda (class)
                (let inner ((others other-productions))
                  (cond
                    ((null? others) #t)
                    ((memq class (car (car others)))
                     (printf 
                       "LL shift conflict for class ~s in non-terminal ~s:~%~s~%~s~%"
                       class non-terminal this-production (car others)))
                    (else (inner (cdr others))))))
              (car this-production))
            ;; and check the others
            (loop other-productions)))))))
                  
      
;;; ****************************************************************        

;;; scan.s

;;; scanner using streams

;;; table ::= ((keyword ...)
;;;            (label (tester ... action) ...)  {last tester may be ELSE}
;;;            ...)

;;; tester ::= CHAR | ALPHABETIC | NUMERIC | ANY
;;;          | (tester ...) | (ARBNO tester)

;;; action ::= #f | label | class

;;; ****************************************************************

;;; syntax

(define-record sllgen:scanner-result (token char stream))
(define-record sllgen:token (class data))

(define sllgen:automaton->start-state caadr)
(define sllgen:automaton->keywords car)
(define sllgen:automaton->table cdr)

(define sllgen:table->state-names (lambda (table) (map car table)))

(define sllgen:arbno->tester cadr)

;;; ****************************************************************

;;; interpreter for automata

(define sllgen:trace? #f)

(define sllgen:apply-automaton
  (lambda (automaton state buffer char stream)
    (let* ((start-state (sllgen:automaton->start-state automaton))
           (keywords    (sllgen:automaton->keywords    automaton))
           (table       (sllgen:automaton->table       automaton))
           (states      (sllgen:table->state-names     table)))
;      (printf "state = ~s~%" state)
      (cond
        ((eq? state #f)
         ;; state is #f: this is a bogus item.  Erase the buffer and try
         ;; again. 
         (sllgen:apply-automaton automaton
           (sllgen:automaton->start-state automaton)
           '() char stream))
        ((not (memq state states))
         ;; This must be a class-- Emit a token
         (let ((token
                 (sllgen:cook-token
                   (reverse buffer) state keywords)))
           (if sllgen:trace? (printf "~d~%" token))
           (make-sllgen:scanner-result 
             token char stream)))
        (else
          (let alternative-loop
            ((alternatives (cdr (assq state table))))
;            (printf " ~s~%" alternatives)
            (cond
              ((null? alternatives)
               (error 'sllgen:apply-automaton
                 "illegal character in state ~s: ~s"
                 state char))
              ((and (null? (cdr alternatives))
                    (eq? (caar alternatives) 'else))
               (sllgen:scanner-apply-action (cdr (car alternatives))
                 automaton buffer char stream))
              ((sllgen:apply-tester (car (car alternatives)) char)
               (sllgen:scanner-apply-action (car alternatives)
                 automaton buffer char stream))
              (else 
                (alternative-loop (cdr alternatives))))))))))

(define sllgen:scanner-apply-action
  (lambda (action automaton buffer char stream)
    (cond
      ((null? action)
       (error 'sllgen:scanner-apply-action "null action!"))
      ((null? (cdr action))
       (sllgen:apply-automaton
         automaton (car action) buffer char stream))
      ((sllgen:arbno? (car action))
       (if (sllgen:apply-tester
             (sllgen:arbno->tester (car action))
             char)
         ;; this char belongs to the arbno
         (sllgen:stream-get stream
           (lambda (first rest)
             (sllgen:scanner-apply-action action automaton 
               (cons char buffer) first rest)))
         ;; no, it doesn't belong-- go on
         (sllgen:scanner-apply-action
           (cdr action) automaton buffer char stream)))
      ((sllgen:apply-tester (car action) char)
       (sllgen:stream-get stream
        (lambda (first rest)
          (sllgen:scanner-apply-action (cdr action)
            automaton (cons char buffer) first rest))))
      (else
        (error 'sllgen:scanner-apply-action "regexp ~s~%failed on input ~s"
          action char)))))


;;; ****************************************************************

;;; Cooking a token

(define sllgen:cook-token
  (lambda (char-list cooker keywords)
    (case cooker
      ((identifier) 
       (let ((sym 
               (string->symbol
                 (list->string char-list))))
         (if (memq sym keywords)
           (make-sllgen:token sym #f)
           (make-sllgen:token 'identifier sym))))
      ((number)
       (make-sllgen:token 'number
         (string->number (list->string char-list))))
      (else
	(if (symbol? cooker)
	  (make-sllgen:token cooker #f)
	  (else (error 'apply-cooker
		  "unknown cooker ~s" cooker)))))))

;;; ****************************************************************

;;; Testers

;; tester ::= CHAR | WHITESPACE | ALPHABETIC | NUMERIC | ANY
;;           |  (char ...) | (ARBNO tester)

(define sllgen:apply-tester
  (lambda (tester ch)
    (cond
      ((char? tester) (char=? tester ch))
      ((symbol? tester)
	(case tester
	  ((whitespace) (char-whitespace? ch))
	  ((alphabetic) (char-alphabetic? ch))
	  ((numeric) (char-numeric? ch))
          ((any) #t)                    ; ELSE is not really a tester
	  (else (error 'sllgen:apply-tester
                  "unknown tester ~s" tester))))
      ((sllgen:arbno? tester)
       #t)                              ; arbno always succeeds
      (else                             
        ;;it's a list of testers -- take the OR of them
        (let loop ((testers tester))
          (and (not (null? testers))
               (or (sllgen:apply-tester (car testers) ch)
                   (loop (cdr testers)))))))))


;;; ****************************************************************

;;; scaffolding

(define sllgen:automaton->stream-transducer
  (lambda (automaton)
    (letrec
      ((loop (lambda (char stream)      ; char * stream -> token * token-stream
                 (let
                   ((next-result
                      (sllgen:apply-automaton automaton
                        (sllgen:automaton->start-state automaton)
                        '() char stream)))
                   (let ((next-token
                           (sllgen:scanner-result->token next-result))
                         (next-char
                           (sllgen:scanner-result->char next-result))
                         (next-stream
                           (sllgen:scanner-result->stream next-result)))
                     (cons next-token
                           (lambda ()
                             (loop next-char next-stream))))))))
      (lambda (stream)
        (lambda ()
          (sllgen:stream-get stream loop))))))

;;; interface for other programs:

(define sllgen:string->token-stream
  (lambda (automaton string)
    (let ((transducer (sllgen:automaton->stream-transducer automaton))
          (stream (sllgen:string->stream string)))
      (transducer stream))))

;;; for top-level testing:

(define sllgen:string->token-list
  (lambda (automaton string)
    (sllgen:stream->list
      (sllgen:string->token-stream automaton string)
      (lambda (token)
        (eq? (sllgen:token->class token) 'end-marker)))))


;;; ****************************************************************

;;; streams

(define sllgen:stream-get
  (lambda (stream rcvr)
    (let ((the-pair (stream)))
      (rcvr (car the-pair) (cdr the-pair)))))

(define sllgen:stream->list
  (lambda (stream end-of-stream?)
    (sllgen:stream-get stream
      (lambda (val newstream)
        (if (end-of-stream? val) '()
          (cons val
                (sllgen:stream->list newstream end-of-stream?)))))))

(define sllgen:string->stream
  (lambda (str)
    (let ((length (string-length str)))
      (letrec 
        ((chars-from (lambda (i)
                       (lambda ()
                         (cons
                           (if (>= i length)
                             #\^
                             (string-ref str i))
                           (chars-from (+ i 1)))))))
        (chars-from 0)))))

;;; see tests.s for examples.

;;; ****************************************************************        

;;; parse.s

;;; parse.s -- run the generated parser

;;; parsing table is of following form:

;;; table ::= ((non-terminal alternative ...) ...)
;;; alternative ::= (list-of-items action ...) 
;;; action ::= (TERM symbol) | (NON-TERM symbol) | (GOTO symbol) 
;;;            | (EMIT-LIST) | (REDUCE symbol)

;;; sllgen:string->stream and sllgen:stream->list are defined in scan.s

(define sllgen:token-stream-get sllgen:stream-get)

;;; The token register can either contain an token or '() -- the latter
;;; signifying an empty buffer, to be filled when necessary.

(define-record sllgen:parser-result (tree token stream))

(define sllgen:find-production
  (lambda (non-terminal parser buf token stream)
    (if (null? token)
      (sllgen:stream-get stream
        (lambda (next-token next-stream)
          (set! token next-token)
          (set! stream next-stream))))
    '(printf "sllgen:find-production: non-terminal = ~s token = ~s~%"
      non-terminal token)
    (let loop
      ((alternatives (cdr (assq non-terminal parser))))
      (cond
        ((null? alternatives)
         (error 'sllgen:find-production
          "~s can't begin with ~s"
          non-terminal
          (sllgen:token->class token)))
        ((memq (sllgen:token->class token) (car (car alternatives)))
         '(printf "sllgen:find-production: using ~s~%~%"
           (cdr (car alternatives)))
         (sllgen:apply-actions (cdr (car alternatives))
           parser buf token stream))
        (else (loop (cdr alternatives)))))))

(define sllgen:apply-actions
  (lambda (action-list parser buf token stream)
    (let loop ((actions action-list)
               (buf buf)
               (token token)
               (stream stream))
    (let ((action (car actions))
          (next-action (cdr actions)))
      '(printf "action-list = ~s~%token = ~s buf = ~s~%~%"
        action-list token buf)
      (case (car action)
        ((term)
         (let ((class (cadr action)))
           (if (null? token)
             (sllgen:stream-get stream
               (lambda (next-token next-stream)
                 (set! token next-token)
                 (set! stream next-stream))))
           (if (eq? (sllgen:token->class token)
                    class)
             (loop next-action 
               (if (sllgen:token->data token)
                 (cons (sllgen:token->data token) buf)
                 buf)
               '()
               stream)
             (error 'sllgen:apply-actions
               "looking for ~s in ~s, found ~s"
               class action-list token))))
        ((non-term)
         (let ((non-terminal (cadr action)))
           (let ((next-result
                   (sllgen:find-production non-terminal parser
                     '() token stream)))
             (let ((tree (sllgen:parser-result->tree next-result))
                   (token (sllgen:parser-result->token next-result))
                   (stream (sllgen:parser-result->stream next-result)))
               (loop next-action (cons tree buf) token stream)))))
        ((goto)
         (let ((non-terminal (cadr action)))
           (sllgen:find-production non-terminal parser buf token stream)))
        ((emit-list)
         (make-sllgen:parser-result
           (reverse buf)
           token 
           stream))
        ((reduce)
         (let ((symbol (cadr action)))
           (make-sllgen:parser-result
             (apply (make-record-from-name symbol)
                    (reverse buf))
             token
             stream)))
        (else
          (error 'sllgen:apply-actions
            "unknown instruction ~s"
            action)))))))
          
(define sllgen:parse-top-level
  (lambda (parser start-symbol token-stream)
    (let ((result 
            (sllgen:find-production start-symbol parser
              '() '() token-stream)))
      (record-case result
        (sllgen:parser-result (tree token stream)
          (let ((token (if (null? token)
                        (sllgen:token-stream-get
                          stream (lambda (token stream) token))
                        token)))
            (if (eq? (sllgen:token->class token) 'end-marker)
              tree
;             (pretty-print tree)
              (error 'sllgen:parse-top-level
                "symbols left over: ~s..."
                token))))
        (else 
          (error 'sllgen:parse-top-level
            "top-level-parse not a parser-result"))))))

;;; ****************************************************************        

;;; tests.s

;;; ****************************************************************

;;; test languages for LL(1) parser generator

;;; ****************************************************************

;;; Example 1:  commands

(define sllgen:automaton-1
  '((begin end exit)                    ; keywords
    (start-state
      (whitespace #f)
      (alphabetic identifier-state)
      (numeric (arbno numeric) number)
      (#\+  plus-sym)
      (#\:  assign-sym-state)
      (#\%  comment-state)
      (#\;  semicolon)
      (#\(  lparen)
      (#\)  rparen)
      (#\^  end-marker))
    (identifier-state 
      ((alphabetic numeric) identifier-state)
      (else identifier))
    (assign-sym-state
      (#\= assign-sym)
      (else identifier-state))
    (comment-state 
      (#\newline #f)
      (any comment-state))))

(define sllgen:grammar-1
    '((command
        (begin (arbno command semicolon) end)
        compound-command)
      (command
        (identifier assign-sym expression)
        assignment-command)
      (expression
        (identifier)
        var-expression)
      (expression
        (number)
        const-expression)
      (expression
        (lparen expression plus-sym expression rparen)
        addition-expression)))

(define sllgen:test-1 '*)

(define sllgen:gen-1
  (lambda ()
    (set! sllgen:test-1 (sllgen:string->tree sllgen:automaton-1
                   sllgen:grammar-1)))) 

;;; ****************************************************************

(define sllgen:automaton-2
  '((proc if then else let set! in =)   ; keywords
    (start-state
      (whitespace #f)
      ((alphabetic  #\* #\+ #\- #\/ #\! #\= #\:)
       (arbno (numeric alphabetic #\* #\+ #\- #\/ #\! #\= #\:))
       identifier)
      ((numeric #\-)
       (arbno numeric)
       number)
      (#\( lparen)
      (#\) rparen)
      (#\^ end-marker)
      (#\% comment-state))
    (comment-state
        (#\newline #f)
        (any comment-state))))

(define sllgen:grammar-2
  '((expression
      (number)
      lit-exp)
    (expression
      (identifier)
      var-exp)
    (expression
      (set! identifier expression)
      assign-exp)
    (expression
      (if expression then else expression)
      if-exp)
    (expression
      (let (arbno declaration) in expression)
      let-exp)
    (expression
      (proc lparen (arbno identifier) rparen expression)
      proc-exp)
    (expression
      (lparen expression (arbno expression) rparen)
      app-exp)
    (declaration
      (identifier = expression)
      decl)))
       
(define sllgen:test-2 '*)

(define sllgen:gen-2
  (lambda ()
    (set! sllgen:test-2 (sllgen:string->tree sllgen:automaton-2
                   sllgen:grammar-2)))) 
  

;;; sample transcript.

; (load "all.scm")
; > (sllgen:gen-1)
; > (sllgen:test-1 "begin x:=y; u := vv; z := (x + y) end")

; Error in sllgen:apply-actions: looking for semicolon in ((non-term command) (term semicolon) (goto #:g67)), found (token end #f).
; ;;; this is right-- the grammar specifies semicolon-terminated commands
; Type (debug) to enter the debugger.
; > (sllgen:test-1 "begin x:=y; u := vv; z := (x + y); end")
; (compound-command
;    ((assignment-command x (var-expression y))
;     (assignment-command u (var-expression vv))
;     (assignment-command
;        z
;        (addition-expression
;           (var-expression x)
;           (var-expression y)))))
; > (sllgen:gen-2)
; > (sllgen:test-2 "let x = (+ y z) u = (f v) in (g x y z)")
; (let-exp
;    ((decl x (app-exp (var-exp +) ((var-exp y) (var-exp z))))
;     (decl u (app-exp (var-exp f) ((var-exp v)))))
;    (app-exp (var-exp g) ((var-exp x) (var-exp y) (var-exp z))))
; > 




