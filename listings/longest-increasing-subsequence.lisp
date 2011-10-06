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
  
(defun explode-candidates-subsequences (lst)
  (if (null lst)
    (quote (()))
    (find-valid-subsequences (explode-candidates-subsequences (cdr lst)) (car lst))))

(defun find-longest-increasing-subsequences (lst)
  (find-longest-increasing-subsequences-internal (explode-candidates-subsequences lst) 0 (quote ())))
    
(defun find-longest-increasing-subsequences-internal (lst max maxSubsequences)
  (if (null lst)
    maxSubsequences
    (if (< (length (car lst)) max)
      (find-longest-increasing-subsequences-internal (cdr lst) max maxSubsequences)
      (if (> (length (car lst)) max)
        (find-longest-increasing-subsequences-internal (cdr lst) (length (car lst)) (cons (car lst) (quote ())))
        (find-longest-increasing-subsequences-internal (cdr lst) max (cons (car lst) maxSubsequences))))))
