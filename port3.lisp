(in-package :clim-graphic-forms)

(defclass graphic-forms-pane-mixin (standard-full-mirrored-sheet-mixin)
  ())

(defclass gfw-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   ;; these two may be surplus?
   (x :initform 0)
   (y :initform 0)))

(defclass graphic-forms-port (standard-event-port-mixin standard-port)
  ((screen
    :accessor graphic-forms-port-screen
    :initform nil)
   (pointer 
    :reader port-pointer
    :initform nil)))

(defun parse-graphic-forms-server-path (path)
  path)

;;; FIXME: if :port-type and :server-path-parser aren't CLIM-specified
;;; keywords, they should be altered to be in some mcclim-internal
;;; package instead.
(setf (get :graphic-forms :port-type) 'graphic-forms-port)
(setf (get :graphic-forms :server-path-parser) 'parse-graphic-forms-server-path)

;;;
;;; port methods
;;;

(defmethod initialize-instance :after ((port graphic-forms-port) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value port 'pointer) (make-instance 'gfw-pointer :port port)
        (port *sheet-dispatcher*) port
	(graphic-forms-port-screen port) (<+ `(make-instance 'gfw::root-window)))
  (push (make-instance 'graphic-forms-frame-manager :port port)
        (slot-value port 'climi::frame-managers))
  (make-graft port)
  (when clim-sys:*multiprocessing-p*
    (setf (climi::port-event-process port)
    	  (clim-sys:make-process
    	   (lambda ()
    	     (loop
    		(with-simple-restart
                 (restart-event-loop
                  "Restart CLIM's event loop.")
    		  (loop
    		     (process-next-event port)))))
    	   :name (format nil "~S's event process." port)))))

;;;
;;; mirror methods
;;;

(defmethod port-set-mirror-region ((port graphic-forms-port) (mirror gfw-top-level) region)
  (let ((size (<+ `(gfs:make-size :width ,(floor (bounding-rectangle-width region))
				  :height ,(floor (bounding-rectangle-height region))))))
    (<+ `(setf (gfw:size ,mirror) (gfw::compute-outer-size ,mirror ,size)))))

(defmethod port-set-mirror-region ((port graphic-forms-port) (mirror gf-mirror-mixin) region)
  (<+ `(setf (gfw:size ,mirror)
	     (gfs:make-size :width ,(floor (bounding-rectangle-width region))
			    :height ,(floor (bounding-rectangle-height region))))))

(defmethod port-set-mirror-transformation ((port graphic-forms-port) (mirror gf-mirror-mixin) transformation)
  (multiple-value-bind (x y)
      (transform-position transformation 0 0)
    (<+ `(setf (gfw:location ,mirror)
	       (gfs:make-point :x ,(floor x)
			       :y ,(floor y))))))


;;; sheet methods

(defmethod realize-mirror ((port graphic-forms-port) (sheet graphic-forms-pane-mixin))
  (%realize-mirror port sheet))

(defmethod %realize-mirror ((port graphic-forms-port) (sheet climi::top-level-sheet-pane))
  (let* ((q (compose-space sheet))
	 (mirror (<+ `(make-instance 'gfw-top-level
				     :sheet ,sheet
				     :dispatcher *sheet-dispatcher*
				     :style '(:workspace)
				     :text ,(frame-pretty-name (pane-frame sheet))
				     ;; TODO in GRAPHIC-FORMS: this minsize is client size, should convert to window size
				     :minimum-size ,(requirement->size q)))))
    (climi::port-register-mirror (port sheet) sheet mirror)
    mirror))

(defmethod %realize-mirror ((port graphic-forms-port) (sheet climi::unmanaged-top-level-sheet-pane))
  (let* ((q (compose-space sheet))
	 (mirror (<+ `(make-instance 'gfw-top-level
				     :sheet ,sheet
				     :dispatcher *sheet-dispatcher*
				     :style '(:borderless)
				     :text ,(frame-pretty-name (pane-frame sheet))
				     :minimum-size ,(requirement->size q)))))

    (climi::port-register-mirror (port sheet) sheet mirror)
        (port-enable-sheet port sheet)
    mirror))

(defmethod %realize-mirror ((port graphic-forms-port) (sheet basic-sheet))
  "%REALIZE-MIRROR for all other sheets (except methods specified above."
  (let* ((parent (sheet-mirror (sheet-parent sheet)))
         (mirror (<+ `(make-instance 'gfw-panel
				     :sheet ,sheet
				     :dispatcher *sheet-dispatcher*
				     :style '()
				     :parent ,parent))))
    (climi::port-register-mirror (port sheet) sheet mirror)
    mirror))

(defmethod destroy-mirror ((port graphic-forms-port) (sheet standard-full-mirrored-sheet-mixin))
  (let ((mirror (climi::port-lookup-mirror port sheet)))
    (climi::port-unregister-mirror port sheet mirror)
    (<- `(gfs:dispose ,mirror))))


;;; pixmap

(defmethod realize-mirror ((port graphic-forms-port) (pixmap climi::pixmap))
  (when (null (climi::port-lookup-mirror port pixmap))
    (let* ((pix (<+ `(make-instance 'gfg:image
				    :size
				    (gfs:make-size
				     :width ,(round (pixmap-width pixmap))
				     :height ,(round (pixmap-height pixmap)))))))
      (climi::port-register-mirror port pixmap pix))
    (values)))

(defmethod destroy-mirror ((port graphic-forms-port) (pixmap climi::pixmap))
  (when (climi::port-lookup-mirror port pixmap)
    (<- `(gfs:dispose ,(climi::port-lookup-mirror port pixmap)))
    (climi::port-unregister-mirror port pixmap (climi::port-lookup-mirror port pixmap))))

(defmethod port-allocate-pixmap ((port graphic-forms-port) sheet width height)
  (let ((pixmap (make-instance 'climi::mirrored-pixmap
			       :sheet sheet
			       :width width
			       :height height
			       :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port graphic-forms-port) pixmap)
  (when (climi::port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))


(defmethod port-enable-sheet ((port graphic-forms-port) (sheet standard-full-mirrored-sheet-mixin))
  (<- `(gfw:show ,(climi::port-lookup-mirror port sheet) t)))

(defmethod port-disable-sheet ((port graphic-forms-port) (sheet standard-full-mirrored-sheet-mixin))
  (<- `(gfw:show ,(climi::port-lookup-mirror port sheet) nil)))

(defmethod destroy-port :before ((port graphic-forms-port))
  (<+ `(gfs:dispose ,(graphic-forms-port-screen port)))
  (close-graphic-forms-server))

;This function and (setf port-motion-hints) are performance plus specific to CLX, not useful on Windows
(defmethod port-motion-hints ((port graphic-forms-port) (sheet standard-full-mirrored-sheet-mixin))
  ())

(defmethod (setf port-motion-hints)
    (value (port graphic-forms-port) (sheet standard-full-mirrored-sheet-mixin))
  value)

(defmethod get-next-event ((port graphic-forms-port) &key wait-function (timeout nil))
  (declare (ignore wait-function timeout))
  (server-get-event))

(defmethod process-next-event :after ((port graphic-forms-port) &key wait-function (timeout nil))
  (declare (ignore wait-function timeout))
  ;; (bt:with-lock-held (*mediums-to-render-lock*)
  ;;   (when *mediums-to-render*
  ;;     (debug-prin1 "process-next-event :after" *mediums-to-render*)))
;  (render-pending-mediums)
  )

(defmethod make-graft ((port graphic-forms-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'graphic-forms-graft
			       :port port :mirror (gensym)
			       :orientation orientation :units units)))
    (setf (sheet-region graft)
	  (make-bounding-rectangle 0 0
				   (graft-width graft)
				   (graft-height graft)))
    (push graft (climi::port-grafts port))
    graft))

(defmethod make-medium ((port graphic-forms-port) sheet)
  (make-instance 'graphic-forms-medium :port port :sheet sheet))

(defmethod text-style-mapping
    ((port graphic-forms-port) text-style &optional character-set)
  ())

(defmethod (setf text-style-mapping)
    (font-name (port graphic-forms-port)
     (text-style text-style) &optional character-set)
  ())

(defmethod port-character-width ((port graphic-forms-port) text-style char))

(defmethod port-string-width ((port graphic-forms-port) text-style string &key (start 0) end))

(defmethod port-mirror-width ((port graphic-forms-port) (sheet standard-full-mirrored-sheet-mixin))
  (let ((mirror (climi::port-lookup-mirror port sheet)))
    (<+ `(gfs:size-width (gfw:size ,mirror)))))

(defmethod port-mirror-height ((port graphic-forms-port) (sheet standard-full-mirrored-sheet-mixin))
  (let ((mirror (climi::port-lookup-mirror port sheet)))
    (<+ `(gfs:size-height (gfw:size ,mirror)))))

(defmethod port-mirror-width ((port graphic-forms-port) (sheet graphic-forms-graft))
  (graft-width sheet))

(defmethod port-mirror-height ((port graphic-forms-port) (sheet graphic-forms-graft))
  (graft-height sheet))

(defmethod graft ((port graphic-forms-port))
  (first (climi::port-grafts port)))


(defmethod pointer-position ((pointer gfw-pointer))
  (values (slot-value pointer 'x) (slot-value pointer 'y)))

(defmethod pointer-button-state ((pointer gfw-pointer))
  ())

(defmethod port-modifier-state ((port graphic-forms-port))
  ())

;;; Set the keyboard input focus for the port.

(defmethod port-frame-keyboard-input-focus
    ((port graphic-forms-port) frame)
  ;; fixme
  (frame-properties frame 'focus))

(defmethod (setf port-frame-keyboard-input-focus)
    (focus (port graphic-forms-port) frame)
  (<+ `(gfw:give-focus ,(sheet-mirror focus)))
  (setf (frame-properties frame 'focus) focus))

(defmethod port-force-output ((port graphic-forms-port))

  (render-pending-mediums)
  )

;;; Windows' event model is different from X that, on windows, event is processed per application (globally),
;;; but not dispatch to clients (except button and menu). So we only need to set what is grabbed and in our
;;; manual event dispatch, check whether grabbed and use different dispatch strategy.
;;; in order to make grab and ungrab work correctly in recursive tracking-pointer, we use push and pop here.
(defmethod port-grab-pointer ((port graphic-forms-port) pointer sheet)
  (declare (ignore pointer))
  (push sheet (pointer-grab-sheet port)))

(defmethod port-ungrab-pointer ((port graphic-forms-port) pointer sheet)
  (declare (ignore sheet pointer))
  (pop (pointer-grab-sheet port)))

(defmethod set-sheet-pointer-cursor ((port graphic-forms-port) sheet cursor)
  ())        

(defmethod bind-selection ((port graphic-forms-port) window &optional time)
  ())

(defmethod release-selection ((port graphic-forms-port) &optional time)
  ())

(defmethod request-selection ((port graphic-forms-port) requestor time)
  ())

(defmethod get-selection-from-event ((port graphic-forms-port) event)
  ())

(defmethod send-selection ((port graphic-forms-port) event string)
  nil)

(defun get-modifier-state ()
  (logior (if (gfw:key-down-p gfw:+vk-shift+)
	      +shift-key+
	      0)
	  (if (gfw:key-down-p gfw:+vk-alt+)
	      +meta-key+
	      0)
	  (if (gfw:key-down-p gfw:+vk-control+)
	      +control-key+
	      0)))

(defun get-mouse-button-state ()
  (cond ((gfw:key-down-p gfw:+vk-lbutton+)
	 +pointer-left-button+)
	((gfw:key-down-p gfw:+vk-mbutton+)
	 +pointer-middle-button+)
	((gfw:key-down-p gfw:+vk-rbutton+)
	 +pointer-right-button+)))


(defmethod synthesize-pointer-motion-event ((pointer gfw-pointer))
  (let* ((port (port pointer))
	 (sheet (climi::port-pointer-sheet port)))
    (when sheet
      (let ((mirror (sheet-direct-mirror sheet)))
	(when mirror
	  (debug-prin1 "synthesize" mirror)
	  (let* ((pointer-pos (gfw:obtain-pointer-location))
		 (x (gfs::point-x pointer-pos))
		 (y (gfs::point-y pointer-pos)))
	    (make-instance
	     'pointer-motion-event
	     :pointer 0 :button (get-mouse-button-state)
	     :x x :y y
					;:x x :y y TODO: x y here is actually graft-x graft-y
					;	   :graft-x root-x
					;	   :graft-y root-y
	     :sheet sheet
	     :modifier-state (get-modifier-state))))))))
