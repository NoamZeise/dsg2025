(in-package :dsg2025)

(defclass world-object (fw:object) ())

(defun make-world-object (meshes)
  (make-instance
   'world-object
   :meshes meshes))
