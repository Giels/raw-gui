(asdf:defsystem #:raw-gui
 :description "autowrapped imgui + cl-sdl2 = raw-gui"
 :author "Giels"
 :license "BSD 2 Clause"
 :serial t
 :depends-on (:sdl2 :cl-opengl :autowrapped-cimgui
 		:trivial-garbage :livesupport
		:cl-autowrap
		:bodge-glad)
 :components
 ((:file "package")
  (:file "library")
  (:file "raw-gui")))
