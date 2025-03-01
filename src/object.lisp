(in-package :dsg2025)

(defclass world-object (fw:object) ())

(defun make-world-object (meshes)
  (make-instance
   'world-object
   :meshes meshes
   :diffuse nil))

;; ui object

(defclass ui-object (fw:object)
  ())

(defun make-ui-object (rect tex)
  (make-instance
   'ui-object
   :meshes (list (fw:get-asset 'quad))
   :diffuse (list tex)
   :model (gficl:2d-rect-matrix rect)))
