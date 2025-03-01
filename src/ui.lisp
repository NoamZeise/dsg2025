(in-package :dsg2025)

(defclass ui (game-scene)
  ())

(defun make-ui (world-fb)
  (make-instance 'ui :scene (make-ui-scene world-fb)))

(defun make-ui-scene (world-fb)
  (let ((scene (make-instance 'ui-scene))
	(obj (make-ui-object (gficl:make-vec '(10 10 100 100))
			     (cons :tex (fw:get-asset 'test))))
	(world-view (make-ui-object (gficl:make-vec '(100 10 200 200))
				    (cons :id world-fb))))
    (fw:resize scene +ui-res-w+ +ui-res-h+)
    (update-scene scene (list obj world-view))
    scene))

(defmethod update ((ui ui) dt))
