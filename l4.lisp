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
(defparameter *deriv-rules*
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
(defun arbitrary-const? (pat)
  (remove nil
	  (map 'list
	       (lambda (x)
		 (if (and (consp x)
			  (string= (car x) "?C"))
		     x
		     ())) pat)))

(defun arbitrary-var? (pat)
  (remove nil
	  (map 'list
	       (lambda (x)
		 (if (and (consp x)
			  (string= (car x) "?V"))
		     x
		     ())) pat)))

(defun arbitrary-exp? (pat)
  (remove nil
	  (map 'list
	       (lambda (x)
		 (if (and (consp x)
			  (string= (car x) "?"))
		     x
		     ())) pat)))

(defun const? (exp)
  (numberp exp))

(defun var? (exp)
  (symbolp exp))

(defun empty-dict ()
  '())

(defun var-name (pat)
  (cadar (arbitrary-var? pat)))
  

(defun extend-dict (pat dat dict)
  (let* ((name (var-name pat))
	;; assq searches alist dict for value stored in key 'name'
	(v (assoc name dict)))
    (cond ((null v)
	   (acons name dat dict))
	  ((eq (cadr v) dat) dict)
	  (t 'failed))))

(defun lookup (var dict)
  (let ((v (assoc var dict)))
    (if (null v) var (cadr v))))

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

(defun instantiate (skel dict)
  (labels ((tree-walk (s)
	     (cond ((not (consp s)) s)
		   ((skeleton-evaluation? s)
		    (evaluate (eval s) dict))
		   (t (cons (tree-walk (car s))
			    (tree-walk (cdr s)))))))
    (tree-walk skel)))

;; What does he mean by "user-initial-enviroment"
;; (defun evaluate (form dict)
;;   (if (not (consp form))      
;;       (lookup form dict)
;;       (apply
;;        (eval (lookup (car form) dict)
;; 	     ;;:user-initial-enviroment)

(defun simplify-exp (exp)
  (try-rules
   (if (consp exp)
       (map 'list #'simplify-exp exp))))

(defun scan (rules)
  (if (null rules)
      exp
      ;; dict is equals the match of the pattern of the top rule, exp
      ;; and the given dict
      (let ((dict (match (caar rules) exp (empty-dict))))
	(if (eq dict 'failed)
	    (scan (cdr rules)
		  ;; Simplify the expression resulted by instantiating
		  ;;  the skeleton of the top rule given the dict
		  (simplify-exp (instantiate (cadar rules)
					     dict)))))))
(defun try-rules (exp)
  (scan *deriv-rules*))
				       
		  
		    
					  
	   
