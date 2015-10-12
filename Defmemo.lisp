;;;; oh, huh, it's possible to make a macro that allows you to define automatically memoized functions, isn't it?


;; this isn't pretty but it does, in fact, work
(defmacro defmemo (name args &body body)
  (let* ((g (gensym)))
    `(let ((,g (make-hash-table)))
       (defun ,name ,args
	 (if (gethash ',args ,g)
	     (gethash ',args ,g)
	     (setf (gethash ',args ,g) (progn ,@body)))))))
      
