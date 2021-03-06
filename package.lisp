(defpackage #:imgui
  (:use #:cl)
  (:nicknames "im"))

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
	#:display-size
	#:display-rate
    #:pos
    #:title
    #:init-fn
    #:render-fn
    #:event-fn

    ;; Methods
    #:update-size
    #:update-pos

	;; Macros
	#:with-panel
	#:with-tab-bar
	#:with-tab

	;; InputText helpers
	#:text-user-data
	#:str
	#:str-len
	#:resize-string

    ;; Main
    #:open-window
    #:main-loop
    #:run))
