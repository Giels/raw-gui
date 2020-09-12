(in-package :raw-gui)

(defparameter *frametime-target* (* (/ 1.0 60.0) 1000)) ;; in ms
(defparameter *internal-time-ms-scale* (/ internal-time-units-per-second 1000))

(defclass window ()
  ((wnd :initform nil :reader sdl2-window)
   (ctx :initform nil :reader gl-context)
   (fn :initform (lambda () (values)) :initarg :fn :accessor fn)
   (vsync :initform t :initarg :vsync :reader vsync)
   (size :initform '(800 600) :initarg :size :reader size)
   (pos :initform '(0 0) :initarg :pos :reader pos)
   (title :initform "raw-gui window" :initarg :title :reader title)))

(defmethod open-window ((this window))
  (with-slots (wnd ctx vsync size pos title) this
    (setf wnd
	  (sdl2:create-window :title title
			      :x (first pos) :y (second pos) :w (first size) :h (second size)
			      :flags '(:shown :opengl :resizable)))
    (sdl2:gl-set-attr sdl2-ffi:+sdl-gl-doublebuffer+ 1)
    (sdl2:gl-set-attr sdl2-ffi:+sdl-gl-context-flags+ 0)
    (sdl2:gl-set-attr sdl2-ffi:+sdl-gl-context-major-version+ 3)
    (sdl2:gl-set-attr sdl2-ffi:+sdl-gl-context-minor-version+ 2)
    (setf ctx
	  (sdl2:gl-create-context wnd))
    (sdl2:gl-make-current wnd ctx)
    (gl:viewport 0 0 (first size) (second size))
    (gl:clear-color 0.0 0.0 0.0 1.0)
    (when vsync (sdl2:gl-set-swap-interval 1))
    (let ((gl-ctx (gl-context this))
	  (sdl2-wnd (sdl2-window this)))
      ;; Must close over foreign objects, not over `this'
      (trivial-garbage:finalize this
				(lambda ()
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

(defun sdl-event-data (event &rest struct-fields)
  (apply #'c-plus:c-ref event sdl2-ffi:sdl-event struct-fields))

(defmethod main-loop ((this window))
    (with-slots (wnd ctx fn) this
      (sdl2:in-main-thread
	  (:background nil)
	  (sdl2:with-sdl-event
	    (evt)
	    (let ((quit nil))
	      (loop while (not quit)
		    for iter-start = (current-time-ms)
		    do
		    (loop for rc = (sdl2:next-event evt :poll) 
			  until (= 0 rc)
			  do
			  ;;
			  (let ((event-type (sdl2:get-event-type evt)))
			    (case event-type
			      (:quit (setf quit t))
			      (:keydown (let* ((sym (sdl-event-data evt :key :keysym))
					       (scancode (sdl2:scancode-value sym))
					       (mod-val (sdl2:mod-value sym)))
					  (format t "~a ~a~%" mod-val scancode)))
			      (:windowevent (let ((a (sdl-event-data evt :window :data1))
						  (b (sdl-event-data evt :window :data2))
						  (event (sdl-event-data evt :window :event)))
					      (cond ((= event sdl2-ffi:+sdl-windowevent-size-changed+)
						     (update-size this (list a b)))
						    ((= event sdl2-ffi:+sdl-windowevent-moved+)
						     (update-pos this (list a b)))
						    (t (values)))))
			      (t (values)))))
		    (unless quit
		      (gl:clear :color-buffer :depth-buffer)
		      (livesupport:continuable (funcall fn))
		      (livesupport:update-repl-link)
		      (gl:flush)
		      (sdl2:gl-swap-window wnd)
		      (let ((iter-end (current-time-ms)))
			(when (< (- iter-end iter-start) *frametime-target*)
			  (sdl2:delay (floor (- *frametime-target* (- iter-end iter-start)))))))))))))

(defun run (&rest args)
  (sdl2:make-this-thread-main
    (lambda ()
      ;; It seems that unless with init/close sdl2, some thread-related mishaps prevent clean close/reopen.
      (sdl2:init :video)
      (glad:init)
      (cimgui:create-context)
      (unwind-protect
	(let ((window (apply #'make-instance 'window args)))
	  (open-window window)
	  (main-loop window))
	(progn (trivial-garbage:gc)
	       (trivial-garbage:gc)
	       (sdl2:quit))))))
