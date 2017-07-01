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
;;     (save-png)))
(defun drawline (p1 p2)
  (with-graphics-state
    (move-to (car p1) (cdr p1))
    (line-to (car p2) (cdr p2))
    (stroke)))
