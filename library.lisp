(in-package :raw-gui)

(pushnew (asdf:system-relative-pathname :raw-gui #p"lib/")
	 cffi:*foreign-library-directories*
	 :test #'equal)

(cffi:define-foreign-library sdl2
  (:unix (:or "libSDL2.so" "lib/libSDL2.so"))
  (:windows "SDL2.dll")
  (t (:default "libSDL2.so")))

(cffi:use-foreign-library sdl2)
