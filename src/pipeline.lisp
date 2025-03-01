(in-package :dsg2025)

;; world shader

(defclass world-shader (fw:normals-shader) ())

(defmethod fw:reload ((s world-shader))
  (fw:shader-reload-files (s (#p"world.vs" #p"world.fs")) shader))

(defmethod fw:draw ((s world-shader) scene)
  (gl:enable :depth-test :cull-face)	   
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

;;; post

(defclass post (fw:post-shader) ())

(defmethod fw:reload ((s post))
  (fw:shader-reload-files (s (#p"post.vs" #p"post.fs")) shader
    (gl:uniformi (gficl:shader-loc shader "screen") 0)))

(defmethod fw:shader-scene-props ((s post) (scene post-scene))
  (with-slots (fw:shader) s
    (gficl:bind-matrix fw:shader "transform" (transform scene))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d (fw:get-post-tex scene :world :color-attachment0))))

;;; main pipeline

(defclass post-scene (fw:post-scene)
  ((transform :initform (gficl:make-matrix) :accessor transform)))

(defclass main-pipeline (fw:pipeline)
  ((post-scene :initarg :post-scene)))

(defun make-main-pipeline ()
  (let ((world-pass (make-world-pass))
	(post-scene (make-instance 'post-scene)))
    (fw:resize world-pass +world-res-w+ +world-res-h+)
    (fw:set-post-texs post-scene (list (cons :world (fw:get-textures world-pass))))
    (make-instance
     'main-pipeline
     :passes (list (cons :world world-pass))
     :shaders (list (cons :post (make-instance 'post)))
     :post-scene post-scene)))

(defmethod fw:resize ((pl main-pipeline) (w integer) (h integer))
  (with-slots (post-scene) pl
    (setf (transform post-scene)
	  (gficl:target-resolution-matrix +world-res-w+ +world-res-w+ w h))))

(defmethod fw:draw ((pl main-pipeline) scenes)
  (fw:draw (fw:get-pass pl :world) (get-scene scenes :world))
	   
  (gl:clear-color 0 0 0 0)  
  (gl:bind-framebuffer :framebuffer 0)
  (gl:viewport 0 0 (gficl:window-width) (gficl:window-height))
  (gl:disable :multisample :depth-test :cull-face)
  (gl:clear :color-buffer-bit)
  (with-slots (post-scene) pl    
    (fw:draw (fw:get-shader pl :post) post-scene)))
