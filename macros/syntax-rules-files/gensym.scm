(let ((loop (gensym))) ; gensym if you're not using SCM.
  `(let ,loop ()
     (cond (,condition
            (begin . ,body)
            (,loop)))))
