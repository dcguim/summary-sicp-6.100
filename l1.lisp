;; Sqrt function by black box solving
(defun square-rt (x)		 
  (flet ((good-enough? (g)
	   (< (abs (- (* g g) x))
	      .001))
	 (improve-guess (g)
	   (/ (+ g (/ x g)) 2)))  
    (labels ((try (guess)
	       (if (good-enough? guess)
		   guess
		   (try (improve-guess guess)))))
      (try 1))))

;; Generic sum of integer
;; Recursive sum of integers
(defun sum-int (a b)
  (if (> a b)
      0
      (+ a (sum-int (1+ a) b))))

;; ex: (gen-sum-int 1 3 #'(lambda (x) (* x x)) #'1+)
(defun gen-sum-int (a b term next)
  (if (> a b) 0
      (+ (funcall term a) 
	 (gen-sum-int (funcall next a) b term next))))

;; Generic fixed point function
;; ex:   (defun square-rt (x)
;;	    (fixed-point #'(lambda (y) (/ (+ (/ x y) y) 2))
(defun fixed-point (f start)
  (let ((tolerance 0.00001))
    (flet ((close-enough? (old new)
	     (< (abs (- old new)) tolerance)))
      (labels ((iter (old new)
		 (if (close-enough? old new)
		     new
		     (iter new (funcall f new)))))
	(iter start (funcall f start))))))


