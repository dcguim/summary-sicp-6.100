;; Pattern ----rule-----> Skeleton
;;   |                      |
;; match               instantiation   
;;   |                      |
;;   v                      v
;; Expression             Expression
;; source --------------> target
;; obj: (defun dsimp
;;        (simplifier deriv-rules))
;; > (dsimp '(dd (+ x y) x))
;; (+ 1 0)
(defparameter deriv-rules
  '(
    ((dd (?c c) (? v)) 0)
    ((dd (?v v) (? v)) 1)
    ((dd (?v u) (? v)) 0)
    ((dd (+ (? x1) (? x2)) (? v))
     (+ (dd (res x1) (res v))
      (dd (res x2) (res v))))
    ((dd (* (? x1) (? x2)) (? v))
     (+ (* (res x1) (dd (res x2) (res v)))
      (* (dd (res x1) (res v)) (res x2))))
    ((dd (** (? x) (?c n)) (? v))
     (* (* (res n)
	   (** (res x) (res (- n 1))))
      (dd (res x) (res v))))
    ))
;; Every rule must be checked, if any rule matches, a dictionary is
;; fed to the instantiator which returns an expression back to the
;; matcher, to see if it can be further simplified, until it no longer
;; changes.
;;                    [ rule1 ], [ rule2 ] ...
;;                    | P | S |  | P | S |
;;                      |   |
;;        |-------------|   |-----------|
;;        v                             v
;;  |-->[match]----dictionary---->[instantiator]-|
;;  |                                            |
;;  |--------expression-and-dictionary-----------|

(defun match (pat exp dict)
  (cond ((eq dict 'failed) 'failed)
	((not (consp pat))
	 (if (not (consp exp))
	     (if (eq pat exp)
		 dict
		 'failed)
	     'failed))
	((arbitrary-const? pat)
	 (if (const? exp)
	     (extend-dict pat exp dict)
	     'failed))
	((arbitrary-var? pat)
	 (if (var? exp)
	     (extend-dict pat exp dict)
	     'failed))
	((arbitrary-exp? pat)
	 (extend-dict pat exp dict))	 
	((not (consp exp)) 'failed)
	(t (match (cdr pat)
	     (cdr exp)
	     (match (car pat)
	       (car exp)
	       dict)))))))
