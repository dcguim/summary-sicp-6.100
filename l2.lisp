;; Expect to receive points coordinates x and y
(defun make-vector (x y) (cons x y))

(defun ycor (p) (cdr p))

(defun xcor (p) (car p))

;; Expect to receive two points so it makes a line segment
(defun make-seg (a b) (cons a b))

(defun start-seg (p) (car p))

(defun end-seg (p) (cdr p))

;; Operations on segments
(defun midpoint (p)
  (let ((a (start-seg p))
	(b (end-seg p)))
    (make-vector (/ (+ (xcor a) (xcor b)) 2)
		 (/ (+ (ycor a) (ycor b)) 2))))

(defun length-seg (s)
  (let ((dx (- (xcor (end-seg s))
	       (xcor (start-seg s))))
	(dy (- (ycor (end-seg s))
	       (ycor (start-seg s)))))
    (sqrt (+ (* dx dx) (* dy dy)))))		 

;; So basically we can define rational numbers, segments and vectors out
;; of pairs (cons), but what is cons? Pure abstraction. It returns a
;; closure which will called car and cdr to access the elements of the
;; pairs.

(defun cons1 (a b)
  #'(lambda (pick)
      (cond ((= pick 1) a)
	    ((= pick 2) b))))

(defun car1 (x) (funcall x 1))

(defun cdr1 (x) (funcall x 2))
