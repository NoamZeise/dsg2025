(in-package :dsg2025)

(defun run ()
  (setf trivial-main-thread:*on-error* #'invoke-debugger)
  (trivial-main-thread:with-body-in-main-thread () (program)))

(defun program ()
  (gficl:with-window
   (:title "DSG2025"
    :resize-callback #'resize-callback
    :width 800
    :height 600)
   (setup)
   (loop until (gficl:closedp)
	 do (update)
	 do (render))
   (cleanup)))

(defun setup ()
  (fw:init-watched)
  (load-assets)
  (setf *signal-fn* nil)
  (create-pipelines)
  (create-scenes)
  (resize-callback (gficl:window-width) (gficl:window-height))
  (gl:enable :depth-test :cull-face)
  (gl:front-face :cw)
  (gl:cull-face :front))

(defun load-assets ()
  (fw:setup-asset-table)
  (fw:add-asset 'quad
		(gficl:make-vertex-data
		 (gficl:make-vertex-form
		  (list (gficl:make-vertex-slot 2 :float)
			(gficl:make-vertex-slot 2 :float)))
		 '(((0 0) (0 0))
		   ((1 0) (1 0))
		   ((1 1) (1 1))
		   ((0 1) (0 1)))
		 '(0 3 2 2 1 0)))
  (fw:load-model 'cube #p"cube.obj"))

(defun create-pipelines ()
  (setf *main-pipeline* (make-main-pipeline)))

(defun cleanup-pipelines ()
  (fw:free *main-pipeline*))

(defun create-scenes ()
  (setf *world-scene* (make-instance 'world-scene))
  (let ((wo (make-world-object (fw:get-asset 'cube))))
    (fw:update-model wo (gficl:make-matrix))
    (with-slots (fw:colour) wo
      (setf fw:colour (gficl:make-vec '(1 0 0 1))))
    (update-scene *world-scene* (list wo)))
  (update-scene-cam
   *world-scene*
   (gficl:make-vec '(-20 8 5))
   (gficl:make-vec '(0 0 0))))

(defun cleanup ()  
  (fw:cleanup-assets)
  (cleanup-pipelines))

(defun resize-callback (w h)
  (fw:resize *main-pipeline* w h)
  (fw:resize *world-scene* w h))

(defun update ()
  (gficl:with-update (dt)
    (gficl:map-keys-pressed
     (:escape (glfw:set-window-should-close))
     (:f (gficl:toggle-fullscreen t)))
    
    (cond (*signal-fn*
	   (funcall *signal-fn*)
	   (setf *signal-fn* nil)))
    (cond ((fw:process-watched)
	   (fw:reload *main-pipeline*)
	   (fw:set-all-unmodified)))))

(defun render ()
  (gficl:with-render
   (fw:draw *main-pipeline* (list (cons :world (list *world-scene*))))))

;;; Signal Functions

(defun signal-quit ()
  (glfw:set-window-should-close))

(defun signal-reload ()
  "manually trigger shader reload"
  (fw:set-all-modified))

(defun signal-fn-lambda (fn)
  (setf *signal-fn* fn))

(defmacro signal-fn (&body body)
  "call fn during next update loop"
  `(signal-fn-lambda (function (lambda () ,@body))))

(defun signal-recreate-scenes ()
  (signal-fn (create-scenes)))

(defun signal-recreate-pipelines ()
  (signal-fn (cleanup-pipelines) (create-pipelines)))

;;; Global Variables

(defparameter *main-pipeline* nil)

(defparameter *world-scene* nil)

(defparameter *signal-fn* nil)
