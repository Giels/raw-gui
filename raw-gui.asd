(asdf:defsystem #:raw-gui
 :description "wrapped imgui + cl-sdl2 = raw-gui"
 :author "Giels"
 :license "BSD 2 Clause"
 :serial t
 :depends-on (:sdl2 :cl-opengl :cl-plus-c
 		:trivial-garbage :livesupport
		:bodge-glad
		:cl-autowrap)
 :components
 ((:file "package")
  (:file "library")
  (:file "imgui")
  (:file "raw-gui")))
