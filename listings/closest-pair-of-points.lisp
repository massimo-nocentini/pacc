

;; with the following function we are able to load automatically
;; the lisp-unit framework in order to test our code
;; in order to run the tests perform the following action
;; 1- open the file "lisp-unit.lisp" compile and load it with C-c C-k
;; 2- open this file and load and compile it too
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;(require :lisp-unit "lisp-unit.lisp")
  (use-package :common-lisp-user))

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

(defun make-set-of-point-definition (lst-of-pairs)
  "This function build a plist with only information on ascissa and
ordinata"
  (mapcar (function
	   (lambda (pair) 		; for each pair we return a
					; list more structured
	    (list :x (car pair) :y (car (cdr pair))))) 
	  lst-of-pairs))

(defun sort-by-ascissa (lst)
  "this method return a new list of plists ordered by the :x component"
  (ascending-sort lst (make-x-key-retriever ))) 

(defun sort-by-ordinata (lst)	
  "this method return a new list of plists ordered by the :y component"
  (ascending-sort lst (make-y-key-retriever )))

(defun make-x-key-retriever ()
  "this function return a function that return the :x component of a plist"
  (function 
   (lambda (lst-of-point-info) 
    (getf lst-of-point-info :x))))

(defun make-y-key-retriever ()
  "this function return a function that return the :y component of a plist"
  (function 
   (lambda (lst-of-point-info) 
    (getf lst-of-point-info :y))))

(defun ascending-sort (lst key-retriever-function)
  "This function makes a copy of the list given as argument and sort it in an
ascending order, using the function object key-retriever-function to find
the object to be used from each element to compare for the ordering."
  (sort (copy-list lst) (function <) 
	:key key-retriever-function))

(defun assign-ordinate-position (lst)
  (assign-positions-on (function y-position-component-getter) lst 0))

(defun assign-ascissa-position (lst)
  (assign-positions-on (function x-position-component-getter) lst 0))

(defun assign-positions-on (position-component-getter-function lst pos)
  "This is the motor that recursively scan the lst, visiting each
element in the given order, and assign its position by attach an integer."
  (if (null lst) 
      ;;  base case: we have processed all items, assigning to each one
      ;; its position by a bijection on integers
      lst			     
      (progn
	(funcall position-component-getter-function (car lst) pos)
	(assign-positions-on 
	 position-component-getter-function (cdr lst) (+ pos 1)))))  

(defun x-position-component-getter (plist pos)
  "this method, given a plist representing a point and the position, 
set the :x-position component to position"
   (setf (getf plist :x-position) pos))

(defun y-position-component-getter (plist pos)
 "this method, given a plist representing a point and the position, 
set the :y-position component to position"
   (setf (getf plist :y-position) pos))
			    
;; ---------------------UNIT TESTS----------------------

(lisp-unit:define-test introduce-sorting-info-test 
  ;; this assertion check that on empty list nothing will be added
  (assert-equal () (introduce-sorting-info ()))
  ;; this assertion assure that the information on x and y ordering
  ;; are introduced (but for now initialized to -1 both)
  (assert-equal (list 
		 '(:X 5 :Y 2 :X-POSITION -1 :Y-POSITION -1) 
		 '(:X 1 :Y 10 :X-POSITION -1 :Y-POSITION -1) 
		 '(:X 3 :Y 4 :X-POSITION -1 :Y-POSITION -1)) 
		(introduce-sorting-info 
		 (list 
		  '(:X 5 :Y 2) 
		  '(:X 1 :Y 10) 
		  '(:X 3 :Y 4)))))

(defun make-simple-test-list ()
  "this function create a simple list with 4 point to be used for test
only"
  (list '(5 2) '(1 10) '(3 4)))

(lisp-unit:define-test make-set-of-point-definition-test 
  (assert-equal () (make-set-of-point-definition ()))
  (assert-equal (list 
		 '(:X 5 :Y 2) 
		 '(:X 1 :Y 10) 
		 '(:X 3 :Y 4)) 
		(make-set-of-point-definition 
		 make-simple-test-list)))

(lisp-unit:define-test sort-by-ascissa-test 
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
  (lisp-unit::assert-false (equal
		 (list 
		  '(:X 3 :Y 4 :X-POSITION -1 :Y-POSITION -1)
		  '(:X 1 :Y 10 :X-POSITION -1 :Y-POSITION -1) 
		  '(:X 5 :Y 2 :X-POSITION -1 :Y-POSITION -1)) 
		 (sort-by-ascissa 
		  (list 
		   '(:X 5 :Y 2 :X-POSITION -1 :Y-POSITION -1) 
		   '(:X 1 :Y 10 :X-POSITION -1 :Y-POSITION -1) 
		   '(:X 3 :Y 4 :X-POSITION -1 :Y-POSITION -1))))))

(lisp-unit:define-test sort-by-ordinata-test 
    (assert-equal () (sort-by-ordinata ()))
  (assert-equal (list 
		 '(:X 5 :Y 2 :X-POSITION -1 :Y-POSITION -1)
		 '(:X 3 :Y 4 :X-POSITION -1 :Y-POSITION -1)
		 '(:X 1 :Y 10 :X-POSITION -1 :Y-POSITION -1))
		(sort-by-ordinata
		 (list 
		  '(:X 5 :Y 2 :X-POSITION -1 :Y-POSITION -1)  
		  '(:X 1 :Y 10 :X-POSITION -1 :Y-POSITION -1)  
		  '(:X 3 :Y 4 :X-POSITION -1 :Y-POSITION -1))))
  (lisp-unit::assert-false (equal
		 (list 
		  '(:X 3 :Y 4 :X-POSITION -1 :Y-POSITION -1)
		  '(:X 1 :Y 10 :X-POSITION -1 :Y-POSITION -1) 
		  '(:X 5 :Y 2 :X-POSITION -1 :Y-POSITION -1)) 
		 (sort-by-ordinata 
		  (list 
		   '(:X 5 :Y 2 :X-POSITION -1 :Y-POSITION -1) 
		   '(:X 1 :Y 10 :X-POSITION -1 :Y-POSITION -1) 
		   '(:X 3 :Y 4 :X-POSITION -1 :Y-POSITION -1))))))