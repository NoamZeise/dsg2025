(in-package :dsg2025)

(defclass world-object (fw:object)
  ((mid :accessor mid :initform (gficl:make-vec '(-100.0 -100.0)))))

(defun make-world-object (meshes)
  (make-instance
   'world-object
   :meshes meshes
   :diffuse nil))

;; ui object

(defclass ui-object (fw:object)
  ((rect :initarg :rect)
   (depth :initarg :depth)))

(defun make-ui-object (rect tex depth &optional (colour (gficl:make-vec '(1 1 1 1))))
  (make-instance
   'ui-object
   :meshes (list (fw:get-asset 'quad))
   :diffuse (list tex)
   :model (gficl:2d-rect-matrix rect :depth depth)
   :rect rect :depth depth
   :colour colour))

(defun update-rect (ui-object new-rect)
  (with-slots (rect depth) ui-object
    (setf rect new-rect)
    (fw:update-model
     ui-object (gficl:2d-rect-matrix rect :depth depth))))
