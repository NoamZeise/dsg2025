(in-package :dsg2025)

(defclass world (game-scene)
  ((pos :initform (gficl:make-vec '(-20 5 0)))
   (dir :initform (gficl:make-vec '(1 -0.2 0)))
   (map :initarg :map)))

(defun make-world ()
  (setup-world-scene
   (make-instance
    'world
    :scene (make-world-scene)
    :map (make-map (make-world-tiles)))))

(defun make-world-scene ()
  (let ((world-scene (make-instance 'world-scene)))
    (fw:resize world-scene +world-res-w+ +world-res-h+)
    world-scene))

(defun make-world-tiles ()
  (list (cons :plain `(make-world-object (fw:get-asset 'plain)))
	(cons :start `(make-world-object (fw:get-asset 'plain)))
	(cons :depot `(make-world-object (fw:get-asset 'depot)))
	(cons :boulders `(make-world-object (fw:get-asset 'boulders)))
	(cons :camp `(make-world-object (fw:get-asset 'camp)))))

(defun setup-world-scene (world)
  (with-slots (scene map pos dir) world
    (update-scene
     scene
     (loop for row in (tiles map) nconcing
	   (loop for tile in row collecting
		 (obj tile))))
    (with-slots ((map-pos pos)) map
      (setf pos (map-to-world map-pos 4))
      (setf dir (gficl:make-vec '(0 0 1)))))
  world)

(defmethod update ((world world) dt)
	   (with-slots (pos dir scene) world     
	     (cam-controls world dt)
	     (update-scene-cam scene pos dir)))

;;; Camera Movement

(defun cam-controls (world dt)
  (with-slots (pos dir) world
    (let ((move (cons 0 0))
	  (cam (cons 0 0)))
      (gficl:map-keys-down
       (:w (setf (cdr move) 1))
       (:a (setf (car move) -1))
       (:s (setf (cdr move) -1))
       (:d (setf (car move) 1))
       (:up (setf (cdr cam) 1))
       (:down (setf (cdr cam) -1))
       (:left (setf (car cam) 1))
       (:right (setf (car cam) -1)))
      (setf dir (calc-cam-dir dir cam dt))
      (setf pos (calc-cam-pos pos dir move dt)))))

(defun calc-cam-dir (dir cam dt &key (cam-speed 0.4))
  (let* ((x (gficl:make-unit-quat
	     (* dt cam-speed (car cam)) fw:+world-up+))
	 (left (gficl:normalise (gficl:cross dir fw:+world-up+)))
	 (y (gficl:make-unit-quat
	     (* dt cam-speed (cdr cam)) left))
	 (rot (gficl:*quat x y)))
    (gficl:quat-conjugate-vec rot dir)))

(defun calc-cam-pos (pos dir move dt &key (cam-speed 10))
  (let* ((left (gficl:normalise (gficl:cross dir fw:+world-up+)))
	 (forward (gficl:normalise (gficl:cross fw:+world-up+ left))))
    (gficl:+vec
     pos
     (gficl:*vec (* dt (car move) cam-speed) left)
     (gficl:*vec (* dt (cdr move) cam-speed) forward))))
