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
;; *note that the function above was already defined in lesson 2*
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
;;     (funcall (make-picture (list
;; 			    (list (cons 30 30)
;; 				  (cons 150 270))
;; 			    (list (cons 150 270)
;; 				  (cons 270 30))
;; 			    (list (cons 90 150)
;; 				  (cons 210 150))))
;; 	     (make-rectangle 0 0 1 1))
;;     (save-png f)))
(defun make-picture (seglist)
  (lambda (rect)
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
     seglist)))

;; Draw 'B' and use it in beside
;; (with-open-file (f "~/work/sicp/img.png")
;;   (with-canvas (:width 300 :height 300)
;;     (funcall (make-picture (list
;; 			    (list (cons 60 30)
;; 				  (cons 60 270))
;; 			    (list (cons 60 270)
;; 				  (cons 240 270)
;; 				  (cons 240 190)
;; 				  (cons 60 190))
;; 			    (list (cons 60 190)
;; 				  (cons 240 190)
;; 				  (cons 240 30)
;; 				  (cons 60 30))))
;; 	     (make-rectangle 0 0 1 1))
;;     (save-png f)))
(defun beside (p1 p2 a)
  (lambda (rect)
    (let* ((ax (scale a (horiz rect)))
	   (comp-ax (scale (- 1 a) (horiz rect)))
	   (orig2 (add-seg (origin rect) ax)))
      (funcall p1 (make-rectangle
		   (car (origin rect))
		   (cdr (origin rect))		     
		   ax
		   (vert rect)))
      (funcall p2 (make-rectangle
		   (car orig2)
		   (cdr orig2)
		   comp-ax
		   (vert rect))))))
