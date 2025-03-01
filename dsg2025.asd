;; assume https://github.com/NoamZeise/gficl is cloned in this folder
(load "gficl/gficl.asd")
(load "framework.asd")
(require 'asdf)
(in-package :asdf-user)

(defsystem :dsg2025
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "dsg2025"
  :entry-point "dsg2025:program"
  :depends-on (:framework)
  :components ((:module "src"
		:components
		((:file "package")
		 (:file "main")
		 (:file "generic")
		 (:file "scene")
		 (:file "pipeline")
		 (:file "object")
		 (:file "game")
		 (:file "map")
		 (:file "world")
		 (:file "ui")))))
