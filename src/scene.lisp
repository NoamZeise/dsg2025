(in-package :dsg2025)

(defun get-scene (scenes key)
  (cdr (assoc key scenes)))

;;; World Scene

(defclass world-scene (fw:scene)
  ())

(defmethod fw:resize ((s world-scene) w h)
  (call-next-method)
  (with-slots (fw:projection) s
    (setf fw:projection (gficl:screen-perspective-matrix w h 0.8 0.05))))

(defgeneric update-scene (scene obj))

(defmethod update-scene ((s world-scene) updated-objects)
  (with-slots (fw:objects) s
    (setf fw:objects updated-objects)))

(defgeneric update-scene-cam (scene pos view))

(defmethod update-scene-cam ((s world-scene) (pos gficl:vec) (view-dir gficl:vec))
  (with-slots (fw:view) s
    (setf fw:view (gficl:view-matrix pos view-dir fw:+world-up+))))

;;; Ui Scene

(defclass ui-scene (fw:scene)
  ())


;;; Game Scene

(defclass game-scene ()
  ((scene :initarg :scene :accessor scene)))

(defmethod resize ((gs game-scene) w h)
  (fw:resize (scene gs) w h))
