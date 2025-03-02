(in-package :dsg2025)

(defclass world (game-scene)
  ((pos :initform (gficl:make-vec (list -20 +base-player-height+ 0)))
   (dir :initform (gficl:make-vec '(1 -0.22 0)))
   (base-dir :initform (gficl:make-vec '(1 -0.22 0)))
   (current-rot :initform 0 :accessor player-dir)
   (target-rot :initform 0)
   (target-pos)
   (map :initarg :map)
   (time :initform 0.0)
   (moving :initform nil)
   (sledgemeter :initform 0.0 :accessor sledgemeter)
   (reset-sledgemeter :initform nil)
   (in-pressure :initform nil)
   (reached-goal :initform nil)
   (won :initform nil :accessor won)))

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
	(cons :start `(make-world-object (fw:get-asset 'start)))
	(cons :depot `(make-world-object (fw:get-asset 'depot)))
	(cons :pressure `(make-world-object (fw:get-asset 'pressure)))
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
      (setf dir (gficl:make-vec '(0 0 1))))
    (setf (slot-value world 'target-pos) pos))
  world)

(defmethod update ((world world) dt)
  (with-slots
      (pos dir scene time moving
	   sledgemeter reset-sledgemeter
	   reached-goal won)
      world
    ;;(debug-cam-controls world dt)
    (cond ((not moving)
	   (if reset-sledgemeter
	       (setf sledgemeter 0))
	   (setf reset-sledgemeter nil)
	   (if reached-goal
	       (setf won t))
	   (world-controls world dt)))
    (setf moving nil)
    (setf time (+ time dt))
    (update-world-cam world dt)
    (with-slots ((scene-time time)) scene
      (setf scene-time time))
    (update-scene-cam scene pos dir)))

;;; Camera Movement

(defun rot-in-range (world)
  (with-slots (current-rot target-rot) world
    (> (abs (- current-rot target-rot)) 0.01)))

(defun world-controls (world dt)
  (with-slots (target-pos target-rot map pos reset-sledgemeter reached-goal in-pressure) world
    (gficl:map-keys-pressed
     (:left (setf target-rot (+ target-rot 1)))
     (:right (setf target-rot (- target-rot 1)))
     (:up (if (not (rot-in-range world))
	      (multiple-value-bind
	       (state new-pos tile) (move-on-map map target-rot)
	       (setf in-pressure nil)
	       (ecase state
		      (:success (setf (gficl:vec-ref new-pos 1) (gficl:vec-ref pos 1))
				(setf target-pos new-pos))
		      (:blocked ()))
	       (case tile
		     (:depot (setf reset-sledgemeter t))
		     (:pressure (setf in-pressure t))
		     (:camp (setf reached-goal t)))))))))

(defun update-world-cam (world dt)
  (with-slots (pos target-pos dir base-dir current-rot target-rot time moving sledgemeter in-pressure) world
    (cond ((rot-in-range world)
	   (setf moving t)
	   (setf current-rot (+ current-rot
				(* (signum (- target-rot current-rot))
				   (* (noisy:noise (+ (* time 0.3) -1000.0))
				      dt))))))
    (setf dir (gficl:quat-conjugate-vec
	       (gficl:make-unit-quat (* (/ pi 4) current-rot)
				     fw:+world-up+)
	       base-dir))
    (setf (gficl:vec-ref pos 1)
	  (+ +base-player-height+
	     (- (* 0.5 (noisy:noise (+ (* time 0.3) 100.0)))
		0.25)
	     (* (sin (* time 2)) 0.05)))
    (setf (gficl:vec-ref target-pos 1) (gficl:vec-ref pos 1))
    (let* ((dir (gficl:-vec target-pos pos))
	   (len (gficl:magnitude dir))
	   (rock 0.1)
	   (rock-speed 0.3))
      (cond ((> len 0.1)
	     (setf moving t)
	     (setf dir (gficl:*vec (/ 1 len) dir))
	     (let ((dist (* (noisy:noise
			     (+ 14.8 (* time 0.1)))
			    (* 14 dt))))
	       (setf rock 0.4)
	       (setf rock-speed 0.7)
	       (setf sledgemeter (+ sledgemeter (* dist (if in-pressure 2.0 1.0))))
	       (setf pos
		     (gficl:+vec pos
				 (gficl:*vec dist dir))))))
      (setf (gficl:vec-ref base-dir 1)
	    (+ -0.22 (- (* rock (noisy:noise (+ (* time rock-speed) 1260.0)))
			(/ rock 2)))))))

(defun debug-cam-controls (world dt)
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
