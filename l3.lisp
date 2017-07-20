;; Lesson 3 - Part A
(in-package :cl-user)
(defpackage #:l3
  (:use :cl :vecto))
(in-package :l3)

;; Define letters made by list of segments used in the examples below 
(defparameter *A* (list
		   (list (cons 30 30)
			 (cons 150 270))
		   (list (cons 150 270)
			 (cons 270 30))
		   (list (cons 90 150)
			 (cons 210 150))))
(defparameter *B* (list
		   (list (cons 60 30)
			 (cons 60 270))
		   (list (cons 60 270)
			 (cons 180 270)
			 (cons 240 190)
			 (cons 60 190))
		   (list (cons 60 190)
			 (cons 240 190)
			 (cons 240 30)
			 (cons 60 30))))

;; Operations on lists.
;; It is not pratical to use cons pairs due to their verbosity, so the
;; function list is quite handy to express long lists.
(defun scale-list (s l)
  (if (null l)
      l
      (cons (* s (car l))
	    (scale-list s (cdr l)))))

;; another way of expressing scale-list
(defun map1 (p l)
  (if (null l)
      l
      (cons (funcall p (car l))
	    (map1 p (cdr l)))))

;; Using the vecto library we can express operations on pictures

;;  Vecto simple example of drawing a rectangle
;; (with-open-file (f "~/work/sicp/img.png")
;;   (with-canvas(:width 100 :height 200)
;;     (rectangle 10 20 30 40)
;;     (fill-path)
;;     (save-png f))
;; *note that the function below was already defined in lesson 2*
(defun add-seg (a b)
  (cons (+ (car a) (car b))
	(+ (cdr a) (cdr b))))

(defun make-rectangle (orig-x orig-y width height)
  (list orig-x orig-y width height))

(defun origin (r)
  (if (= 4 (length r))
      (cons (car r) (cadr r))
      (format t "This rectangle is not well defined!")))

(defun horiz (r)
  (if (= 4 (length r))
      (cons (caddr r) 0)
      (format t "This rectangle is not well defined!")))

(defun vert (r)
  (if (= 4 (length r))
      (cons 0 (cadddr r))
      (format t "This rectangle is not well defined!")))

(defun scale (s pair)
  (cons (* s (car pair)) (* s (cdr pair))))

(defun coord-map (rect)
  (lambda (point)
    (add-seg
     (add-seg (scale (car point)
		     (horiz rect))
	      (scale (cdr point)
		     (vert rect)))
     (origin rect))))

;;  Drawing a simple line with vecto
;; (with-open-file (f "~/work/sicp/img.png")
;;   (with-canvas (:width 90 :height 90)
;;     (drawline (cons 20 30) (cons 40 30))
;;     (save-png f)))
(defun drawline (p1 p2)
  (with-graphics-state
    (move-to (car p1) (cdr p1))
    (line-to (car p2) (cdr p2))
    (stroke)))

;; https://en.wikipedia.org/wiki/Bézier_curve
;; http://www.xach.com/lisp/vecto/#curve-to
(defun drawcurve (p1 p2 p3 p4)
  (with-graphics-state
    (move-to (car p1) (cdr p1))
    (curve-to (car p2) (cdr p2)
	      (car p3) (cdr p3)
	      (car p4) (cdr p4))
    (stroke)))

	     

;; Drawing the letter 'A' with make-picture
;; (with-open-file (f "~/work/sicp/img.png")
;;   (with-canvas (:width 300 :height 300)
;;     (funcall (make-picture *A*)
;; 	     (make-rectangle 0 0 1 1))
;;     (save-png f)))
(defun make-picture (seglist)
  (lambda (rect &rest rest)
    (with-graphics-state
      (mapcar 
       (lambda (s)
	 (cond ((= (length  s) 2)		     
		(drawline
		 (funcall (coord-map rect) (car s))
		 (funcall (coord-map rect) (cadr s))))
	       ((= (length s) 4)
		(drawcurve
		 (funcall (coord-map rect) (car s))
		 (funcall (coord-map rect) (cadr s))
		 (funcall (coord-map rect) (caddr s))
		 (funcall (coord-map rect) (cadddr s))))))
       seglist))))

;; Make picture 'A' and 'B' and use it the picture generator function
;; for a 300x300 canvas.
;; (gen-pic (beside (make-picture *A*)
;; 		 (make-picture *B*)
;; 		 0.5))
(defun beside (p1 p2 a)
  "Is there a way to pass the problem of p2 origin x-coord which 
caused this function to receive also width, only to the scope of 
gen-pic? How to make the information of the width useless?"
  (lambda (rect size)
    (let* ((ax (scale a (horiz rect)))
	   (comp-ax (scale (- 1 a) (horiz rect)))
	   (orig2 (add-seg (origin rect) (scale (car size) ax))))
	(funcall p1 (make-rectangle
		     (car (origin rect))
		     (cdr (origin rect))		     
		     (car ax)
		     (cdr (vert rect)))
		 size)
	(funcall p2 (make-rectangle
		     (car orig2)
		     (cdr orig2)
		     (car comp-ax)
		     (cdr (vert rect)))
		 size))))

(defun above (p1 p2 a)
    "Is there a way to pass the problem of p2 origin y-coord which 
caused this function to receive also heigth, only to the scope of 
gen-pic? How to make the information of the heigth useless?"
  (lambda (rect size)
    (let* ((ay (scale a (vert rect)))
	   (comp-ay (scale (- 1 a) (vert rect)))
	   (orig2 (add-seg (origin rect) (scale (cdr size) ay))))      
	(funcall p1 (make-rectangle
		     (car (origin rect))
		     (cdr (origin rect))		     
		     (car (horiz rect))
		     (cdr ay))
		 size)
	(funcall p2 (make-rectangle
		     (car orig2)
		     (cdr orig2)
		     (car (horiz rect))
		     (cdr comp-ay))
		 size))))

(defun gen-pic (cl-img)
  "Create a canvas with size 'width' and 'height', and draws the given
closure"
  (with-open-file (f "~/work/sicp/img.png")
    (let ((width 300) (height 300))
      (with-canvas (:width width :height height)      
	(apply cl-img (list (make-rectangle 0 0 1 1)
	       (cons width height)))
	(save-png f)))))

;; Calling beside recursively with and using gen-pic to generate it.
;; (gen-pic (right-push (make-picture *A*) 3 0.75))
(defun right-push (p n a)
  "Push copies of the picture 'p' to right direction by ratio 'a',
choose the number of recursions needed in 'n'"
  (if (= n 0)
      p
      (beside p (right-push p (- n 1) a) a)))

(defun up-push (p n a)
  "Push copies of the picture 'p' in ascendent direction by ratio 'a',
choose the number of recursions needed in 'n'"
  (if (= n 0)
      p
      (above p (up-push p (- n 1) a) a)))


;; Lesson 3 - Part B
(defun deriv (f)
  (lambda (x)
    (let ((dx 0.000001))
      (/ (- (funcall f (+ x dx))
	    (funcall f x))
	 dx))))

(defun constant? (exp var)
  (and (not (consp exp))
       (not (eq exp var))))

(defun same-var? (exp var)
  (and (not (consp  exp))
       (eq exp var)))

(defun sum? (exp)
	 (eq (car exp) '+))

(defun make-sum (a1 a2)
  (list '+ a1 a2))

(defun product? (exp)
  (and (consp exp)
       (eq (car exp) '*)))

(defun make-product (a1 a2)
  (list '* a1 a2))

;; Note that in this version we only accept two arguments if we want
;; to pass 3x to we should do:
;; (deriv '(+ (+ x x) x) 'x) or (deriv '(* 3 x) 'x)
;; but not (deriv '(+ x x x) 'x)
;; but that can be easily done mapping the input
(defun deriv (exp var)
  (cond ((constant? exp var) 0)
	((same-var? exp var) 1)
	((sum? exp)
	 (let ((a1 (cadr exp))
	       (a2 (caddr exp)))
	   (make-sum (deriv a1 var)
		     (deriv a2 var))))
	((product? exp)
	 (let ((a1 (cadr exp))
	       (a2 (caddr exp)))
	   (make-sum
	    (make-product  (deriv a1 var)
			   a2)
	    (make-product  a1
			 (deriv a2 var)))))
	(t (format t "~s ~s" exp var))))
