(in-package :dsg2025)

(defun get-scene (scenes key)
  (cdr (assoc key scenes)))

(defgeneric update-scene (scene obj))

(defgeneric update-scene-cam (scene pos view))

(defmethod update-scene ((s fw:scene) updated-objects)
  (with-slots (fw:objects) s
    (setf fw:objects updated-objects)))

(defun add-obj-to-scene (scene obj)
  (with-slots (fw:objects) scene
    (setf fw:objects (append fw:objects (list obj)))))

;;; World Scene

(defclass world-scene (fw:scene)
  ((time :initform 0.0)
   (player-pos :initform (gficl:make-vec '(0 0)) :accessor player-pos)))

(defmethod fw:resize ((s world-scene) w h)
  (call-next-method)
  (with-slots (fw:projection) s
    (setf fw:projection (gficl:screen-perspective-matrix w h 0.8 0.05))))

(defmethod update-scene-cam ((s world-scene) (pos gficl:vec) (view-dir gficl:vec))
  (with-slots (fw:view) s
    (setf fw:view (gficl:view-matrix pos view-dir fw:+world-up+))))

(defmethod update-player-pos (scene pos)
	   (let ((p (gficl:make-vec (list (gficl:vec-ref pos 0) (gficl:vec-ref pos 2)))))
	     (setf (player-pos scene) p)))

(defmethod fw:draw ((scene world-scene) shader)
  (fw:shader-scene-props shader scene)
  (with-slots (fw:objects player-pos) scene
    (loop for o in fw:objects when (in-range player-pos (mid o)) do (fw:draw o shader))))

(defun in-range (player-pos tile-pos)
  (< (gficl:magnitude (gficl:-vec player-pos tile-pos)) 60.0))

;;; Ui Scene

(defclass ui-scene (fw:scene)
  ())

(defmethod fw:resize ((s ui-scene) w h)
  (call-next-method)
  (with-slots (fw:projection) s
    (setf fw:projection (gficl:screen-orthographic-matrix w h))))

;;; Game Scene

(defclass game-scene ()
  ((scene :initarg :scene :accessor scene)))

(defmethod resize ((gs game-scene) w h)
  (fw:resize (scene gs) w h))
