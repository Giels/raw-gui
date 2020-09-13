(in-package :raw-gui)

(pushnew (asdf:system-relative-pathname :raw-gui #p"lib/")
	 cffi:*foreign-library-directories*
	 :test #'equal)

(cffi:define-foreign-library sdl2
  (:unix "libSDL2.so")
  (:windows "SDL2.dll")
  (t (:default "libSDL2")))

(cffi:use-foreign-library sdl2)

(cffi:define-foreign-library imgui
  (:unix "libimgui.so")
  (:windows "imgui.dll")
  (t (:default "libimgui")))

(cffi:use-foreign-library imgui)
