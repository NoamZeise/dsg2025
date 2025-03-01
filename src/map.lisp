(in-package :dsg2025)

(defclass world-map ()
  ((pos :initarg :pos :accessor pos)
   (tiles :initarg :tiles :accessor tiles)))

(defclass tile ()
  ((pos :initarg :pos :accessor pos)
   (obj :initarg :obj :accessor obj)))

(defun make-map (tile-objs)
  (multiple-value-bind (tiles start) (make-tiles tile-objs)
    (make-instance 'world-map :tiles tiles :pos start)))

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
