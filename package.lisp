(defpackage #:raw-gui
  (:use #:cl)
  (:nicknames "rg")
  (:export 
    ;; Window class
    :window

    ;; 1/target framerate
    :*frametime-target*

    ;; Window class accessors/readers
    #:sdl2-context
    #:sdl2-window
    #:gl-context
    #:vsync
    #:size
    #:pos
    #:title
    #:fn

    ;; Helpers
    #:sdl-event-data

    ;; Main
    #:open-window
    #:main-loop
    #:run))
