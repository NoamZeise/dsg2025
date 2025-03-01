(in-package :dsg2025)

(defclass ui (game-scene)
  ())

(defun make-ui ()
  (make-instance 'ui :scene (make-ui-scene)))

(defun make-ui-scene ()
  (let ((scene (make-instance 'ui-scene)))
    scene))

(defmethod update ((ui ui) dt))
