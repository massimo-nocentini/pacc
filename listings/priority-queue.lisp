

(defun make-priority-queue ()
  ;; this function simply return an empty list, ready for subsequent
  ;; insertions
  '())

(defun make-map-entry (id key)
  (list id `(:id ,id :key ,key :parent ,nil :successors ,nil)))

(defun insert (queue id key)
  (cons (make-map-entry id key) queue))
