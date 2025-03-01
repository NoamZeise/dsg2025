(in-package :dsg2025)

(defclass game ()
  ((pipeline :initarg :pipeline)
   (world :initarg :world)
   (ui :initarg :ui)))

(defun make-game ()
  (create-pipelines
   (create-scenes
    (make-instance
     'game))))

(defun create-scenes (game)
  (setf (slot-value game 'world) (make-world))
  (setf (slot-value game 'ui) (make-ui))
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
  (with-slots (world) game
    (update world dt)))

(defmethod resize ((game game) w h)
  (with-slots (pipeline world) game
    (fw:resize pipeline w h)))

(defmethod draw ((game game))
  (with-slots (pipeline world ui) game
    (fw:draw
     pipeline
     (list (cons :world (list (scene world)))
	   (cons :ui (list (scene ui)))))))
