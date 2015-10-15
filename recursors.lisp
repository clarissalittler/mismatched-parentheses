;;; Okay, so I cut my teeth in languages that have true pattern matching
;;; and I'm wondering if I can add syntax for that

;;; First off, if we're doing something like an append function with structural recursion
;;; it'll look something like this in base CL

(defun append-1 (list1 list2)
  (cond ((null list1) list2)
	((consp list1) 
	 (cons (car list1) (append-1 (cdr list1) list2)))))
	 
;;; this kind of recursion feels clunky in comparison to the kind of pattern matching
;;; I'm used to. I mean it's not the worst but still. So, instead, what about something like 
;;; (list-case (list)
;;;   (nil case1)
;;;   ((cons x list2) case2))
;;; ===>
;;; (cond ((null list1) case1)
;;;       ((consp list1)
;;;        (let ((x (car list1))
;;;              (list2 (cdr list2)))
;;;         case2

(defun list-case-helper (listname clause body)
  (cond ((equal clause 'nil) `((null ,listname) ,@body))
	((equal (car clause) 'cons) 
	 (let ((headvar (cadr clause))
	       (tailvar (caddr clause)))
	   `((consp ,listname)
	     (let ((,headvar (car ,listname))
		   (,tailvar (cdr ,listname)))
	       ,@body))))))

(defmacro list-case (listname cb1 cb2)
  (destructuring-bind (clause1 &rest body1) cb1
    (destructuring-bind (clause2 &rest body2) cb2
      `(cond ,(list-case-helper listname clause1 body1)
	     ,(list-case-helper listname clause2 body2)))))
  
;;;; This seems to work! I don't know if it was really any better than the original
;;;; buuut, all that being said it might have its place for writing small programs

(defun append-2 (list1 list2)
  (list-case list1
	     (nil list2)
	     ((cons x list1-1) (cons x (append-2 list1-1 list2)))))

;;; So that's cute! I like it. Maybe it doesn't buy much of anything, but at least
;;; it's a proof of concept
