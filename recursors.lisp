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

;;; What about defining algebraic datatypes?
;;; What we'd want is something like a 
;;; (defadt name constructor*)
;;; where constructor
;;; should have a form like 
;;; (name arg1 arg2 arg3) etc. 
;;; we could do things in a couple of ways:
;;; one where we check the type of the constructors and one where we just don't care
;;; and we're essentially just making a Shape of a sum-of-products constructor
;;; with, in a sense, untyped leaves
;;; 
;;; (defadt tree 
;;;    (leaf)
;;;    (node v t1 t2))
;;; should define
;;; operators called 
;;; leaf which takes no arguments and returns (list 'leaf)
;;; and 
;;; node which takes three arguments and returns (list 'node v t1 t2)
;;; then our auto-generated case-statement should be something like
;;; 
;;; (defun tree-case-helper bloobluh)
;;; (defmacro tree-case bleeblahh)
;;;
;;; but I'm not quite feeling up to writing the macro writing code at the moment
;;; so instead let's just write a defadt that makes the constructors and nothing else
;;; this means, in some sense, it's not going to use the name of the type yet 

;; (defmacro defconsmaker (con-name &rest args)
;;   `(defun ,con-name (,@args)
;;      (list ',con-name ,@args)))

;; (defmacro defadt (consname &rest constructors) 
;;   `(progn ,@(dolist (c constructors) 

(defun conshelper (con-name &rest args)
  `(defun ,con-name (,@args)
     (list ',con-name ,@args)))

(defmacro defadt (adtname &rest constructors) 
  `(progn 
     ,@(mapcar #'(lambda (c) (apply #'conshelper c)) constructors)))
;;; again, this doesn't use the adtname yet
;;; but does generate constructor functions for us.
;;; the case discrimination code is going to need to
;;; build a separate clause for each constructor that binds the known arguments of
;;; that constructor
