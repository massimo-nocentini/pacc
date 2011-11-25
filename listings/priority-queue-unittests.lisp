
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :lisp-unit "lisp-unit.lisp"))

(lisp-unit:define-test queue-initialization-functions
  (lisp-unit:assert-equal 
   () 
   (make-priority-queue))

  (lisp-unit:assert-equal 
   '(D (:ID D :KEY 9 :PARENT NIL :SUCCESSORS NIL))
   (make-map-entry 'd 9))

  (lisp-unit:assert-false
   (equal
    '(D (:ID D :KEY 9 :PARENT NIL :SUCCESSORS NIL))
    (make-map-entry 'pi 10)))
  )

(lisp-unit:define-test queue-inserts
  (lisp-unit:assert-equal 
   '((D (:ID D :KEY 3 :PARENT NIL :SUCCESSORS NIL))) 
   (insert (make-priority-queue) 'd 3))

  (lisp-unit:assert-equal 
   '((H (:ID H :KEY 8 :PARENT NIL :SUCCESSORS NIL))
     (U (:ID U :KEY 0 :PARENT NIL :SUCCESSORS NIL))
     (D (:ID D :KEY 3 :PARENT NIL :SUCCESSORS NIL)))
   (let (queue (make-priority-queue)) 
     (setf queue (insert queue 'd 3))
     (setf queue (insert queue 'u 0))
     (setf queue (insert queue 'h 8))
     queue))

  (lisp-unit:assert-equal 
   '(D (:ID D :KEY 9 :PARENT NIL :SUCCESSORS NIL))
   (make-map-entry 'd 9))

  (lisp-unit:assert-false
   (equal
    '(D (:ID D :KEY 9 :PARENT NIL :SUCCESSORS NIL))
    (make-map-entry 'pi 10)))
  )