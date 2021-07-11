
This might help. It is an extend-syntax version of collect which has not been
tested other than to show that it expands into what looks to be the right
thing. The function nth is just like Common Lisp, if you want to use
list-ref, just switch the order of the two arguments. I use `with' clauses
to help build the let bindings. This is the only complicated feature used.
Hope this helps,

John Gateley

; code starts here
; This just converts (v1 v2 ...) to ((v1 (nth 0 tuple)) (v2 (nth 1 tuple)) ...)
; it should always be called w/ second argument of 0

(define make-v-fun
  (lambda (l n)
    (cond ((null? l) ())
          (t (cons `(,(car l) (nth ,n tuple)) (make-v-fun (cdr l) (+ n 1)))))))

; this guy creates the flatmap expression

(extend-syntax (make-flatmaps) (tuple)
  ((make-flatmaps ((vn setn)) (v ...))
   (map (lambda (vn)
          (list v ... vn))
        setn))
  ((make-flatmaps ((vi seti)(vj setj) ...) (v ...))
   (flatmap
     (lambda (vi)
       (make-flatmaps ((vj setj) ...) (v ... vi)))
     seti)))

; this is the main guy

(extend-syntax (collect) (tuple)
  ((collect result ((v1 set1) ...) restriction)
   (with ((let-bindings (make-v-fun '(v1 ...) 0)))
     (map (lambda (tuple)
            (let let-bindings
              result))
          (filter (lambda (tuple)
                    (let let-bindings
                      restriction))
            (make-flatmaps ((v1 set1) ...) ()))))))


