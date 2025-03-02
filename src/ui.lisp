(in-package :dsg2025)

(defclass ui (game-scene)
  ((compass-point :initarg :compass-point)
   (compass :initarg :compass)
   (sledgemeter :initarg :sledgemeter)
   (win :initarg :win)
   (loss :initarg :loss)
   (over :initform nil)
   (time-since-over :initform 0.0)))

(defun make-ui (world-fb)
  (multiple-value-bind
   (scene compass-point compass sledgemeter) (make-ui-scene world-fb)
   (let ((win (make-ui-object
	      (gficl:make-vec (list 0 0 +ui-res-w+ +ui-res-h+))
	      (cons :tex (fw:get-asset 'win))
	      0.99
	      (gficl:make-vec '(1 1 1 0))))
	 (loss (make-ui-object
		(gficl:make-vec (list 0 0 +ui-res-w+ +ui-res-h+))
		(cons :tex (fw:get-asset 'lose))
		0.99
		(gficl:make-vec '(1 1 1 0)))))
     (make-instance
      'ui :scene scene
      :compass-point compass-point
      :compass compass
      :sledgemeter sledgemeter
      :win win :loss loss))))

(defun game-over (ui state)
  (with-slots (scene win loss time-since-over over) ui
    (setf time-since-over 0)
    (setf over t)
    (ecase state
	   (:win (add-obj-to-scene scene win))
	   (:loss (add-obj-to-scene scene loss)))))

(defun make-ui-scene (world-fb)
  (let ((scene (make-instance 'ui-scene))
	(obj (make-ui-object
	      (gficl:make-vec (list 0 0 +ui-res-w+ +ui-res-h+))
	      (cons :tex (fw:get-asset 'background))
	      -0.9))
	(overlay (make-ui-object
		  (gficl:make-vec (list 0 0 +ui-res-w+ +ui-res-h+))
		  (cons :tex (fw:get-asset 'overlay))
		  0.9))
	(hood (make-ui-object
	       (gficl:make-vec (list 0 0 +ui-res-w+ +ui-res-h+))
	       (cons :tex (fw:get-asset 'hood))
	       0.85))
	(world-view (make-ui-object
		     (gficl:make-vec (list 102 6 +world-res-w+  +world-res-h+))
		     (cons :id world-fb) 0))
	(compass
	 (make-ui-object
	  (gficl:make-vec (list 41 129 91 98))
	  (cons :tex (fw:get-asset 'compass-bg))
	  0.1))
	(compass-point
	 (make-ui-object
	  (gficl:make-vec (list 41 129 91 98))
	  (cons :tex (fw:get-asset 'compass-point))
	  0.11))
	(sledgemeter
	 (make-ui-object
	  (gficl:make-vec (list 339 17 16 16))
	  (cons :tex (fw:get-asset 'sledgemeter))
	  0.1)))
    (fw:resize scene +ui-res-w+ +ui-res-h+)
    (update-scene
     scene
     (list
      obj world-view
      compass compass-point
      sledgemeter
      hood overlay))
    (values scene compass-point compass sledgemeter)))


(defun set-compass-dir (ui angle)
  (with-slots (compass-point) ui
    (with-slots (rect depth) compass-point
      (fw:update-model
       compass-point
       (gficl:2d-rect-matrix
	rect :depth depth :rotation (* angle 2 pi)
	:pivot (midpoint rect))))))

(defun set-sledgemeter (ui distance)
  (with-slots (sledgemeter) ui
    (with-slots (rect depth) sledgemeter
      (setf (gficl:vec-ref rect 1)
	    (+ 17 (/ distance 4.8)))
      (fw:update-model
       sledgemeter
       (gficl:2d-rect-matrix
	rect :depth depth :rotation (* distance 0.1)
	:pivot (midpoint rect))))))

(defmethod update ((ui ui) dt)
  (with-slots (win loss time-since-over over) ui
    (cond (over
	   (setf time-since-over (+ time-since-over dt))
	   (set-colour win time-since-over)
	   (set-colour loss time-since-over)))))

(defun set-colour (obj time)
  (with-slots (fw:colour)  obj
    (setf fw:colour
	  (gficl:make-vec
	   (list 1 1 1 (expt (min 1.0 (/ time *time-to-end*)) 2.2))))))
