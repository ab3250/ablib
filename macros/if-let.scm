(defmacro if-let
  "bindings => binding-form test
  If test is true, evaluates then with binding-form bound to the value of 
  test, if not, yields else"
  {:added "1.0"}
  ([bindings then]
   `(if-let ~bindings ~then nil))
  ([bindings then else & oldform]
   (assert-args
     (vector? bindings) "a vector for its binding"
     (nil? oldform) "1 or 2 forms after binding vector"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (bindings 0) tst (bindings 1)]
     `(let [temp# ~tst]
        (if temp#
          (let [~form temp#]
            ~then)
          ~else)))))


  (define-syntax if-let
    (syntax-rules ()
      ((if-let ((var value) ...)
              consequent ...)
       (let ((var value) ...)
         (if (and var ...)
             consequent ...)))))
          
  (define-syntax when-let
    (syntax-rules ()
      ((when-let (binding)
                 body ...)
       (if-let (binding)
               (begin body ...)))))

  (defmacro if-let (bindings &body (then-form &optional else-form))
      (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                               (list bindings)
                               bindings))
           (variables (mapcar #'car binding-list)))
      `(let ,binding-list
         (if (and ,@variables)
             ,then-form
             ,else-form))))
  
  (defmacro when-let (bindings &body forms)
    (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                             (list bindings)
                             bindings))
           (variables (mapcar #'car binding-list)))
      `(let ,binding-list
         (when (and ,@variables)
           ,@forms))))

