(defun push-on-each (lst elem)
  (if (null lst)
    ()
    (if (not (consp (car lst)))
      (cons (cons elem (car lst)) (push-on-each (cdr lst) elem))
      (if (< elem (car (car lst)))
        (cons (cons elem (car lst)) (push-on-each (cdr lst) elem))
        (push-on-each (cdr lst) elem)))))
        
(defun find-valid-subsequences (lst elem)
  (append lst (push-on-each lst elem)))
  
(defun solve-problem (lst)
  (if (null lst)
    (quote (()))
    (find-valid-subsequences (solveProblem (cdr lst)) (car lst))))

(defun find-longest-increasing-subsequence (lst)
  (find-longest-increasing-subsequence-internal (solve-problem lst) 0 (quote ())))
    
(defun find-longest-increasing-subsequence-internal (lst max maxSubsequences)
  (if (null lst)
    maxSubsequences
    (if (< (length (car lst)) max)
      (find-longest-increasing-subsequence-internal (cdr lst) max maxSubsequences)
      (if (> (length (car lst)) max)
        (find-longest-increasing-subsequence-internal (cdr lst) (length (car lst)) (cons (car lst) (quote ())))
        (find-longest-increasing-subsequence-internal (cdr lst) max (cons (car lst) maxSubsequences))))))
