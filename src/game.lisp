(in-package :dsg2025)

(defclass game ()
  ((pipeline :initarg :pipeline)
   (world :initarg :world)
   (ui :initarg :ui)))

(defun make-game ()
  (create-scenes
   (create-pipelines
    (make-instance
     'game))))

(defun create-scenes (game)
  (setf (slot-value game 'world) (make-world))
  (setf (slot-value game 'ui)
	(make-ui
	 (fw:get-pass-texture (fw:get-pass
			       (slot-value game 'pipeline)
			       :world))))
  game)

(defun create-pipelines (game)
  (setf (slot-value game 'pipeline)
	(make-main-pipeline))
  game)

(defun cleanup-pipelines (game)
  (fw:free (slot-value game 'pipeline)))

(defmethod cleanup ((game game))
  (cleanup-pipelines game))

(defmethod reload ((game game))
  (fw:reload (slot-value game 'pipeline)))

(defmethod update ((game game) dt)
  (with-slots (world ui) game
    (update world dt)
    (update ui dt)))

(defmethod resize ((game game) w h)
  (with-slots (pipeline ui) game
    (fw:resize pipeline w h)))

(defmethod draw ((game game))
  (with-slots (pipeline world ui) game
    (fw:draw
     pipeline
     (list (cons :world (list (scene world)))
	   (cons :ui (list (scene ui)))))))
