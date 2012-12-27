(defun comb (m list fn)
  (labels ((comb1 (l c m)
		  (when (>= (length l) m)
		    (if (zerop m) (return-from comb1 (funcall fn c)))
		    (comb1 (cdr l) c m)
		    (comb1 (cdr l) (cons (first l) c) (1- m)))))
    (comb1 list nil m)))

(defun quicksort (list l r col)
  (if (< (length list) 4)
      (sort-triple list col);for now I don't know how to handle this case
      (comb 3 list (lambda (triple)
		     (let* ((sorted-triple (sort-triple triple))
			    (median (cadr sorted-triple))))
		     ))))

(defun sort-triple (triple col)
  "col is a collector that accept the following (list, num of checks, num of swaps)"
  (let ((a1 (car triple))
	(a2 (cadr triple))
	(a3 (caddr triple)))
    (cond
      ((and (< a1 a2) (< a2 a3)) (funcall col (list a1 a2 a3) 2 0))
      ((and (< a1 a2) (>= a2 a3) (< a1 a3)) (funcall col (list a1 a3 a2) 3 2))
      ((and (< a1 a2) (< a2 a3) (>= a1 a3)) (funcall col (list a3 a1 a2) 3 2))
      ((and (>= a1 a2) (< a1 a3)) (funcall col (list a2 a1 a3) 2 1))
      ((and (>= a1 a2) (>= a1 a3) (< a2 a3)) (funcall col (list a2 a3 a1) 3 2))
      ((and (>= a1 a2) (>= a1 a3) (>= a2 a3)) (funcall col (list a3 a2 a1) 3 1)))))

