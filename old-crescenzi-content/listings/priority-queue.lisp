

(defun make-priority-queue ()
  ;; this function simply return an empty list, ready for subsequent
  ;; insertions
  '())

(defun make-map-entry (id key)
  (list id `(:id ,id :key ,key :parent ,nil :successors ,nil)))

(defun insert (queue id key)
  (cons (make-map-entry id key) queue))

(defun find-root (queue)
  ;; here we have to get the CAR first to obtain the only one root,
  ;; which must be an invariant by definition of tree, after we get
  ;; the CDR to exclude the key of the association list, from that CDR
  ;; we get a list which contains our plist, hence we get the CAR to
  ;; get the first and only one plist associated to the key
  (cadar (member-if (lambda (node-description)
	       (null (get-parent-component 
		      ;; here we have to get the CAR because the CDR
		      ;; function return a list containing some other
		      ;; things, in our case the plist
		      (car (cdr node-description))))) 
	     queue)))

(defun get-parent-component (plist)
  (getf plist :parent))