

;; with the following function we are able to load automatically
;; the lisp-unit framework in order to test our code
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :lisp-unit "lisp-unit.lisp")
  (use-package :lisp-unit))

(defun introduce-sorting-info (lst-of-point-definitions)
"This function add the sorting information to be used during the algorithm."
  (mapcar (function 			
	   ;; for each element, which is a plist	
	   ;; we append another plist with the new information on x and y
	   ;; position of the pair in the respective arrays
	   (lambda (point-definition) 
	    (append point-definition 
		    (list :x-position -1 :y-position -1)))) 
	  lst-of-point-definitions))

(define-test introduce-sorting-info-test 
  (assert-equal () (introduce-sorting-info ()))
  (assert-equal (list 
		 '(:X 5 :Y 2 :X-POSITION -1 :Y-POSITION -1) 
		 '(:X 1 :Y 10 :X-POSITION -1 :Y-POSITION -1) 
		 '(:X 3 :Y 4 :X-POSITION -1 :Y-POSITION -1)) 
		(introduce-sorting-info 
		 (list 
		  '(:X 5 :Y 2) 
		  '(:X 1 :Y 10) 
		  '(:X 3 :Y 4)))))

;;--------------------------------------------------------------------------

(defun sort-by-ascissa (lst)
  "this method return a new list of plists ordered by the :x component"
  (ascending-sort (copy-list lst) (make-x-key-retriever ))) 

(defun sort-by-ordinate (lst)	
  "this method return a new list of plists ordered by the :y component"
  (ascending-sort (copy-list lst) (make-y-key-retriever )))

(defun make-x-key-retriever ()
  (function 
   (lambda (lst-of-point-info) 
    (getf lst-of-point-info :x))))

(defun make-y-key-retriever ()
  (function 
   (lambda (lst-of-point-info) 
    (getf lst-of-point-info :y))))

(defun ascending-sort (lst key-retriever-function)
  (sort (copy-list lst) (function <) 
	:key key-retriever-function))

(define-test sort-by-ascissa-test 
  (assert-equal () (sort-by-ascissa ()))
  (assert-equal (list 
		 '(:X 1 :Y 10 :X-POSITION -1 :Y-POSITION -1) 
		 '(:X 3 :Y 4 :X-POSITION -1 :Y-POSITION -1)
		 '(:X 5 :Y 2 :X-POSITION -1 :Y-POSITION -1)) 
		(sort-by-ascissa 
		 (list 
		  '(:X 5 :Y 2 :X-POSITION -1 :Y-POSITION -1)  
		  '(:X 1 :Y 10 :X-POSITION -1 :Y-POSITION -1)  
		  '(:X 3 :Y 4 :X-POSITION -1 :Y-POSITION -1))))
  (assert-false (equal
		 (list 
		  '(:X 3 :Y 4 :X-POSITION -1 :Y-POSITION -1)
		  '(:X 1 :Y 10 :X-POSITION -1 :Y-POSITION -1) 
		  '(:X 5 :Y 2 :X-POSITION -1 :Y-POSITION -1)) 
		 (sort-by-ascissa 
		  (list 
		   '(:X 5 :Y 2) 
		   '(:X 1 :Y 10) 
		   '(:X 3 :Y 4))))))

;; ------------------------------------------------------
(defun make-set-of-point-definition (lst-of-pairs)
  "This function build a plist with only information on ascissa and
ordinata"
  (mapcar (function
	   (lambda (pair) 		; for each pair we return a
					; list more structured
	    (list :x (car pair) :y (car (cdr pair))))) 
	  lst-of-pairs))


(define-test make-set-of-point-definition-test 
  (assert-equal () (make-set-of-point-definition ()))
  (assert-equal (list 
		 '(:X 5 :Y 2) 
		 '(:X 1 :Y 10) 
		 '(:X 3 :Y 4)) 
		(make-set-of-point-definition 
		 (list '(5 2) '(1 10) '(3 4)))))
;; ------------------------------------------------------

(defun assign-ordinate-position (lst pos)
  (if (null lst)
      ()
      (progn
	(setf (getf (car lst) :y-position) pos)
	(assign-ordinate-position (cdr lst) (+ pos 1)))))


(defun assign-ascissa-position (lst pos)
  (if (null lst) 
      ()
      (progn
	(setf (getf (car lst) :x-position) pos)
	(assign-ascissa-position (cdr lst) (+ pos 1)))))

