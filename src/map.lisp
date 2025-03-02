(in-package :dsg2025)

(defclass world-map ()
  ((pos :initarg :pos :accessor pos)
   (tiles :initarg :tiles :accessor tiles)))

(defclass tile ()
  ((pos :initarg :pos :accessor pos)
   (obj :initarg :obj :accessor obj)))

(defun make-map (tile-objs)
  (setf *world-map*
	(loop for row in *map-layout* collecting
	      (loop for tile in row collecting
		    (ecase tile
			   (:b :boulders)
			   (:p :plain)
			   (:r :pressure)
			   (:s :start)
			   (:c :camp)
			   (:d :depot)))))
  
  (multiple-value-bind
   (tiles start) (make-tiles tile-objs)
   (make-instance 'world-map :tiles tiles :pos start)))

(defun move-on-map (map dir)
  (ecase (mod dir 4)
	 (0 (move-on-map-dir map '(1 0)))
	 (1 (move-on-map-dir map '(0 -1)))
	 (2 (move-on-map-dir map '(-1 0)))
	 (3 (move-on-map-dir map '(0 1)))))

(defun move-on-map-dir (map dir)
  (destructuring-bind (x y) dir
    (with-slots (pos) map
      (destructuring-bind (m-x . m-y) pos
        (let* ((new-pos (cons (+ x m-x) (+ y m-y)))	       
	       (tile (get-tile new-pos)))
	  (ecase tile
		 ((:plain :start :depot :pressure :camp)		       
		  (setf pos (cons (+ x m-x) (+ y m-y)))		       
		  (values :success (map-to-world pos) tile))
		 (:boulders (values :blocked pos tile))))))))

(defun get-tile (pos)
  (let ((target nil))
    (loop for y from 0 for row in *world-map* do
	  (loop for x from 0 for tile in row when
		(and (= x (car pos))
		     (= y (cdr pos)))
		return (setf target tile)))
    target))

(defun make-tiles (tile-objs)
  (let ((start '(0 0)))
    (values
     (loop for y from 0 for row in *world-map* collecting
	   (loop for x from 0 for tile in row collecting
		 (progn
		   (if (equalp tile :start)
		       (setf start (cons x y)))
		   (make-instance
		    'tile
		    :obj (make-tile-obj
			  (eval (get-tile-expr tile tile-objs)) x y)
		    :pos (cons x y)))))
     start)))

(defun get-tile-expr (tile tile-objs)
  (let ((tile-al (assoc tile tile-objs)))
    (assert tile-al () "map key was invalid ~a" tile)
    (cdr tile-al)))

(defun make-tile-obj (game-obj x y)
  (fw:update-model
   game-obj
   (gficl:translation-matrix (map-to-world (cons x y))))
  game-obj)

(defun map-to-world (map-pos &optional (z 0))
  (gficl:make-vec (list (* 20 (car map-pos))
			z
			(* 20 (cdr map-pos)))))
