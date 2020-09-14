(ql:quickload :raw-gui)

(defparameter *ui-state* (make-hash-table))

(defun init (wnd)
  (declare (ignore wnd))
  (if *ui-state*
    (clrhash *ui-state*)
    (setf *ui-state* (make-hash-table)))
  (setf (gethash :checkbox-state *ui-state*) (cffi:foreign-alloc :bool :initial-element 0 :count 1))
  (setf (gethash :float-state *ui-state*) (cffi:foreign-alloc :float :initial-element 0.5 :count 1)))

(defun ui-fn (wnd)
  (cffi:with-foreign-object (size '(:struct imgui::ImVec2))
			    (cffi:with-foreign-slots ((imgui::x imgui::y) size (:struct imgui::ImVec2))
						     (setf imgui::x (float (first (raw-gui:size wnd))))
						     (setf imgui::y (float (second (raw-gui:size wnd))))
						     (imgui::SetNextWindowSize size)))
  (cffi:with-foreign-array (pos #(0.0 0.0) '(:array :float 2))
			   (imgui::SetNextWindowPos pos))
  (imgui::Begin3 "Hello, world!" (cffi:null-pointer) (apply #'logior (mapcar (lambda (x) (cffi:foreign-enum-value 'imgui::WindowFlags_ x)) (list :NoSavedSettings :NoMove :NoResize :NoTitleBar :AlwaysAutoResize :NoCollapse))))
  (imgui::Text "Lorem Ipsum, etc.")

  (let ((prev-state (cffi:mem-aref (gethash :checkbox-state *ui-state*) :bool 0)))
    (imgui::Checkbox "Click me!" (gethash :checkbox-state *ui-state*))
    (unless (eq prev-state (cffi:mem-aref (gethash :checkbox-state *ui-state*) :bool 0))
      (format t "You did it! ~a -> ~a~%" prev-state (not prev-state))))

  (imgui::SliderFloatMinMax "Float Slider" (gethash :float-state *ui-state*) 0.0 1.0)
  (imgui::SameLine)
  (when (imgui::Button "Bump!")
    (format t "ZWAP~%"))
  (imgui::Text (format nil "Checkbox: ~a" (cffi:mem-aref (gethash :checkbox-state *ui-state*) :bool 0)))
  (imgui::End))

(defun cleanup ()
  (loop for k being the hash-key of *ui-state* do
	 (cffi:foreign-free (gethash k *ui-state*))
	 (remhash k *ui-state*)))

(defun handle-events (wnd evt)
  (let ((event-type (sdl2:get-event-type evt)))
	(case event-type
	  (:quit nil)
	  (:keydown (let* ((sym (plus-c:c-ref evt sdl2-ffi:sdl-event :key :keysym))
					   (scancode (sdl2:scancode-value sym))
					   (mod-val (sdl2:mod-value sym)))
				  (format t "~a ~a~%" mod-val scancode)
				  t))
	  (t t))))

(unwind-protect
    (raw-gui:run :vsync t :size '(800 600) :pos '(0 0) :title "Demo" :render-fn #'ui-fn :event-fn #'handle-events :init-fn #'init)
  (cleanup))
