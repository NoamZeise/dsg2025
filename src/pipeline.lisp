(in-package :dsg2025)

;; world shader

(defclass world-shader (fw:normals-shader) ())

(defmethod fw:reload ((s world-shader))
  (fw:shader-reload-files (s (#p"world.vs" #p"world.fs")) shader
    (gl:uniformi (gficl:shader-loc shader "tex") 0)
    (gl:uniformi (gficl:shader-loc shader "use_texture") 0)))

(defmethod fw:draw ((s world-shader) scene)
  (call-next-method))

(defmethod fw:shader-scene-props ((s world-shader) (scene world-scene))
  (with-slots (fw:shader) s
    (with-slots (fw:view fw:projection) scene
      (gficl:bind-matrix fw:shader "view" fw:view)
      (gficl:bind-matrix fw:shader "projection" fw:projection))))

(defmethod fw:shader-mesh-props ((s world-shader) props)
  (with-slots (fw:shader) s
      (gficl:bind-vec fw:shader "obj_colour" (cdr (assoc :colour props)))))

;; world pass

(defclass world-pass (fw:pass) ())

(defun make-world-pass ()
  (make-instance
   'world-pass
   :shaders (list (make-instance 'world-shader))
   :description
   (fw:make-framebuffer-description
    (list (gficl:make-attachment-description :type :texture)
	  (gficl:make-attachment-description :position :depth-attachment))
    :samples (gficl:msaa-samples 8))
   :clear-colour '(1 1 1 0)))

;;; main pipeline

(defclass main-pipeline (fw:pipeline)
  ())

(defun make-main-pipeline ()
  (make-instance
   'main-pipeline
   :passes (list (cons :world (make-world-pass)))))

(defmethod fw:draw ((pl main-pipeline) scenes)
  (fw:draw (fw:get-pass pl :world) (get-scene scenes :world))
  (gficl:blit-framebuffers
   (fw:get-final-framebuffer (fw:get-pass pl :world)) nil
   (gficl:window-width) (gficl:window-height)))
