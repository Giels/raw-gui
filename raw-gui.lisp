(in-package :raw-gui)

(defparameter *frametime-target* (* (/ 1.0 60.0) 1000)) ;; in ms
(defparameter *internal-time-ms-scale* (/ internal-time-units-per-second 1000))

(defclass window ()
  ((wnd :initform nil :reader sdl2-window)
   (ctx :initform nil :reader gl-context)
   (imgui-ctx :initform nil :reader imgui-ctx)
   (init-fn :initform (lambda (wnd) (values)) :initarg :init-fn :accessor init-fn)
   (render-fn :initform (lambda (wnd) (values)) :initarg :render-fn :accessor render-fn)
   (event-fn :initform (lambda (wnd evt) t) :initarg :event-fn :accessor event-fn)
   (vsync :initform t :initarg :vsync :reader vsync)
   (size :initform '(800 600) :initarg :size :reader size)
   (pos :initform '(0 0) :initarg :pos :reader pos)
   (display-size :initform '(0 0) :reader display-size)
   (display-rate :initform 0 :reader display-rate)
   (title :initform "raw-gui window" :initarg :title :reader title)))

(defmethod open-window ((this window))
  (with-slots (wnd ctx imgui-ctx vsync size display-size display-rate pos title) this
	(setf wnd
		  (sdl2:create-window :title title
							  :x (first pos) :y (second pos) :w (first size) :h (second size)
							  :flags '(:shown :opengl :resizable)))
	(sdl2:gl-set-attr sdl2-ffi:+sdl-gl-doublebuffer+ 1)
	(sdl2:gl-set-attr sdl2-ffi:+sdl-gl-context-flags+ 0)
	(sdl2:gl-set-attr sdl2-ffi:+sdl-gl-context-major-version+ 3)
	(sdl2:gl-set-attr sdl2-ffi:+sdl-gl-context-minor-version+ 2)
	(sdl2:gl-set-attr sdl2-ffi:+sdl-gl-context-forward-compatible-flag+ 1) ;; Apparently needed for OSX
	(multiple-value-bind (_ w h rate)
	  (sdl2:get-current-display-mode 0)
	  (setf display-size (list w h))
	  (setf display-rate rate)
	  (when (> rate 0)
		(setf *frametime-target* (* (/ 1.0 rate) 1000.0))))
    (setf ctx
	  (sdl2:gl-create-context wnd))
    (sdl2:gl-make-current wnd ctx)
    (setf imgui-ctx (imgui::CreateContext))
    (glad:init)
    (imgui::ImplSDL2_InitForOpenGL (autowrap:ptr wnd) (autowrap:ptr ctx))
    (cffi:with-foreign-string (str "#version 150\0")
			      (imgui::ImplOpenGL3_Init))
    (gl:viewport 0 0 (first size) (second size))
    (gl:clear-color 0.0 0.0 0.0 1.0)
    (when vsync (sdl2:gl-set-swap-interval 1))
    (let ((gl-ctx (gl-context this))
	  (sdl2-wnd (sdl2-window this))
	  (ig-ctx (imgui-ctx this)))
      ;; Must close over foreign objects, not over `this'
      (trivial-garbage:finalize this
				(lambda ()
				  (imgui::ImplOpengl3_Shutdown)
				  (imgui::ImplSDL2_Shutdown)
				  (imgui::DestroyContext)
				  (sdl2:gl-delete-context gl-ctx)
				  (sdl2:destroy-window sdl2-wnd))))))

(defmethod (setf vsync) (new-vsync (this window))
  (with-slots (ctx) this
    (setf vsync new-vsync)
    (when ctx
      (sdl2:gl-set-swap-interval (if new-vsync 1 0)))))

(defmethod update-size ((this window) new-size)
  (setf (slot-value this 'size) new-size))

(defmethod update-pos ((this window) new-pos)
  (setf (slot-value this 'pos) new-pos))

(defmethod (setf size) (new-size (this window))
  (with-slots (wnd ctx size) this
    (setf size new-size)
    (when wnd
      (sdl2:set-window-size wnd (first new-size) (second new-size)))))

(defmethod (setf pos) (new-pos (this window))
  (with-slots (wnd pos) this
    (setf pos new-pos)
    (when wnd
      (sdl2:set-window-position wnd (first new-pos) (second new-pos)))))

(declaim (inline current-time-ms))
(defun current-time-ms ()
  (coerce (/ (get-internal-real-time) *internal-time-ms-scale*) 'single-float))

(defmethod main-loop ((this window))
  (with-slots (wnd ctx render-fn event-fn init-fn) this
    (sdl2:in-main-thread
      (:background nil)
	  (funcall init-fn this)
      (sdl2:with-sdl-event
		(evt)
		(let ((quit nil))
		  (loop while (not quit)
				for iter-start = (current-time-ms)
				do
				(loop for rc = (sdl2:next-event evt :poll) 
					  until (= 0 rc)
					  do
					  (let ((event-type (sdl2:get-event-type evt)))
						(case event-type
						  (:windowevent (let ((a (plus-c:c-ref evt sdl2-ffi:sdl-event :window :data1))
											  (b (plus-c:c-ref evt sdl2-ffi:sdl-event :window :data2))
											  (event (plus-c:c-ref evt sdl2-ffi:sdl-event :window :event)))
										  (cond ((= event sdl2-ffi:+sdl-windowevent-size-changed+)
												 (update-size this (list a b)))
												((= event sdl2-ffi:+sdl-windowevent-moved+)
												 (update-pos this (list a b)))
												(t (values)))
										  t))))
					  (imgui::ImplSDL2_ProcessEvent (autowrap:ptr evt))
					  (setf quit (not (sdl2:in-main-thread (:background nil) (funcall event-fn this evt)))))
				(unless quit
				  (imgui::ImplOpenGL3_NewFrame)
				  (imgui::ImplSDL2_NewFrame (autowrap:ptr wnd))
				  (imgui::NewFrame)

				  (livesupport:continuable (sdl2:in-main-thread (:background nil) (funcall render-fn this)))
				  (livesupport:update-repl-link)

				  (imgui::Render)

				  (gl:clear :color-buffer :depth-buffer)
				  (imgui::ImplOpenGL3_RenderDrawData (imgui::GetDrawData))
				  (sdl2:gl-swap-window wnd)

				  (let ((iter-end (current-time-ms)))
					(when (< (- iter-end iter-start) *frametime-target*)
					  (sdl2:delay (floor (- *frametime-target* (- iter-end iter-start)))))))))))))

(defun run (&rest args)
  (sdl2:make-this-thread-main
    (lambda ()
      ;; It seems that unless with init/close sdl2, some thread-related mishaps prevent clean close/reopen.
      (sdl2:init :video)
      (unwind-protect
	(let ((window (apply #'make-instance 'window args)))
	  (open-window window)
	  (main-loop window))
	(progn (trivial-garbage:gc)
	       (trivial-garbage:gc)
	       (sdl2:quit))))))

(defmacro with-panel ((name &optional open-ptr flags) &body body)
  (let ((ptr-token (gensym))
		(flags-token (gensym)))
	`(let ((,ptr-token (or ,open-ptr (cffi:null-pointer)))
		   (,flags-token (if ,flags
						   (apply #'logior
								  (mapcar (lambda (x)
											(cffi:foreign-enum-value 'imgui::WindowFlags_ x))
										  ,flags))
						   (cffi:foreign-enum-value 'imgui::WindowFlags_ :None))))
	   (ImGui::Begin3 ,name ,ptr-token ,flags-token)
	   ,@body
	   (ImGui::End))))

(defmacro with-tab-bar ((name &optional flags) &body body)
  (let ((flags-token (gensym)))
	`(let ((,flags-token (if ,flags
						   (apply #'logior
								  (mapcar (lambda (x)
											(cffi:foreign-enum-value 'imgui::TabBarFlags_ x))
										  ,flags))
						   (cffi:foreign-enum-value 'imgui::TabBarFlags_ :None))))
	   (when (ImGui::BeginTabBar2 ,name ,flags-token)
		 ,@body
		 (ImGui::EndTabBar)))))

(defmacro with-tab ((name &optional open-ptr flags) &body body)
  (let ((ptr-token (gensym))
		(flags-token (gensym)))
	`(let ((,ptr-token (or ,open-ptr (cffi:null-pointer)))
		   (,flags-token (if ,flags
						   (apply #'logior
								  (mapcar (lambda (x)
											(cffi:foreign-enum-value 'imgui::TabItemFlags_ x))
										  ,flags))
						   (cffi:foreign-enum-value 'imgui::TabItemFlags_ :None))))
	   (when (ImGui::BeginTabItem3 ,name ,ptr-token ,flags-token)
		 ,@body
		 (ImGui::EndTabItem)))))

(cffi:defcstruct text-user-data
  (str :pointer)
  (str-len :int))

(cffi:defcallback resize-string :int ((data :pointer))
				  (cffi:with-foreign-slots ((imgui::EventFlag imgui::BufTextLen imgui::Buf imgui::UserData) data imgui::InputTextCallbackData)
										   (when (= imgui::EventFlag (cffi:foreign-enum-value 'imgui::InputTextFlags_ :CallbackResize))
											 (cffi:with-foreign-slots ((str str-len) imgui::UserData text-user-data)
																	  (let ((new-str (cffi:foreign-alloc :char :initial-contents 0 :count imgui::BufTextLen)))
																		(cffi:foreign-funcall "strcpy" :pointer new-str :pointer str :pointer)
																		(cffi:foreign-free str)
																		(setf str new-str)
																		(setf str-len imgui::BufTextLen))))))
