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
	 do (update-step)
	 do (render))
   (cleanup-program)))

(defun setup ()
  (fw:init-watched)
  (load-assets)
  (setf *signal-fn* nil)
  (setf *game* (make-game))
  (resize-callback (gficl:window-width) (gficl:window-height))
  (gl:cull-face :front))

(defun load-assets ()
  (fw:setup-asset-table)
  (fw:add-asset 'quad
		(gficl:make-vertex-data
		 (gficl:make-vertex-form
		  (list (gficl:make-vertex-slot 2 :float)))
		 '(((0 0)) ((1 0)) ((1 1)) ((0 1)))
		 '(0 3 2 2 1 0)))
  (fw:load-model 'cube #p"cube.obj")
  (fw:load-image 'test #p"test.png"))

(defun cleanup-program ()  
  (fw:cleanup-assets)
  (cleanup *game*))

(defun resize-callback (w h)
  (resize *game* w h))

(defun update-step ()
  (gficl:with-update (dt)
    (gficl:map-keys-pressed
     (:escape (glfw:set-window-should-close))
     (:f (gficl:toggle-fullscreen)))
    (update *game* dt)
    (cond (*signal-fn*
	   (funcall *signal-fn*)
	   (setf *signal-fn* nil)))
    (cond ((fw:process-watched)
	   (reload *game*)
	   (fw:set-all-unmodified)))))

(defun render ()
  (gficl:with-render
   (draw *game*)))

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
  (signal-fn (create-scenes *game*)))

(defun signal-recreate-pipelines ()
  (signal-fn (cleanup-pipelines *game*) (create-pipelines *game*)))

;;; Global Variables

(defparameter *game* nil)

(defparameter *signal-fn* nil)
