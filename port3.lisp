(in-package :clim-graphic-forms)

(defclass graphic-forms-pane-mixin (standard-single-mirrored-sheet-mixin)
  ())

(defclass gfw-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   ;; these two may be surplus?
   (x :initform 0)
   (y :initform 0)))

(defclass graphic-forms-port (standard-handled-event-port-mixin standard-port)
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

;;;
;;; sheet methods
;;;

(defmethod realize-mirror ((port graphic-forms-port) (sheet climi::top-level-sheet-pane))
  (let* ((mirror (<+ `(make-instance 'gfw-top-level
				     :sheet ,sheet
				     :dispatcher ,*sheet-dispatcher*
				     :style '(:workspace)
				     :text ,(frame-pretty-name (pane-frame sheet))))))
    (climi::port-register-mirror (port sheet) sheet mirror)
    mirror))

(defmethod realize-mirror ((port graphic-forms-port) (sheet climi::unmanaged-top-level-sheet-pane))
  (let* ((parent (sheet-mirror (sheet-parent sheet)))
         (mirror (<+ `(make-instance 'gfw-panel
				     :sheet ,sheet
				     :dispatcher ,*sheet-dispatcher*
				     :style '()
				     :parent ,parent))))
    (climi::port-register-mirror (port sheet) sheet mirror)
    mirror))

(defmethod destroy-mirror ((port graphic-forms-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (climi::port-lookup-mirror port sheet)))
    (climi::port-unregister-mirror port sheet mirror)
    (<- `(gfs:dispose ,mirror))))

(defmethod port-enable-sheet ((port graphic-forms-port) (sheet mirrored-sheet-mixin))
  (<- `(gfw:show ,(climi::port-lookup-mirror port sheet) t)))

(defmethod port-disable-sheet ((port graphic-forms-port) (sheet mirrored-sheet-mixin))
  (<- `(gfw:show ,(climi::port-lookup-mirror port sheet) nil)))

(defmethod destroy-port :before ((port graphic-forms-port))
  (<+ `(gfs:dispose ,(graphic-forms-port-screen port)))
  (close-graphic-forms-server))

;This function and (setf port-motion-hints) are performance plus specific to CLX, not useful on Windows
(defmethod port-motion-hints ((port graphic-forms-port) (sheet mirrored-sheet-mixin))
  ())

(defmethod (setf port-motion-hints)
    (value (port graphic-forms-port) (sheet mirrored-sheet-mixin))
  value)

(defmethod get-next-event ((port graphic-forms-port) &key wait-function (timeout nil))
  (declare (ignore wait-function timeout))
  (server-get-event))

(defmethod process-next-event :after ((port graphic-forms-port) &key wait-function (timeout nil))
  (declare (ignore wait-function timeout))
  (render-pending-mediums))

(defmethod make-graft ((port graphic-forms-port) &key (orientation :default) (units :device))
  (let ((result (make-instance 'graphic-forms-graft
			       :port port :mirror (gensym)
			       :orientation orientation :units units)))
    result))

(defmethod make-medium ((port graphic-forms-port) sheet)
  (make-instance 'graphic-forms-medium :port port :sheet sheet))

(defmethod text-style-mapping
    ((port graphic-forms-port) text-style &optional character-set)
  ())

(defmethod (setf text-style-mapping)
    (font-name (port graphic-forms-port)
     (text-style text-style) &optional character-set)
  ())

(defmethod port-character-width ((port graphic-forms-port) text-style char)
  #+nil (<- `(gfs::debug-format "port-character-width called: ~a ~c~%" ,text-style ,char)))

(defmethod port-string-width ((port graphic-forms-port) text-style string &key (start 0) end)
  #+nil (<- `(gfs::debug-format "port-string-width called: ~a ~c~%" ,text-style ,string)))

(defmethod port-mirror-width ((port graphic-forms-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (climi::port-lookup-mirror port sheet)))
    (<+ `(gfs:size-width (gfw:size ,mirror)))))

(defmethod port-mirror-height ((port graphic-forms-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (climi::port-lookup-mirror port sheet)))
    (<+ `(gfs:size-height (gfw:size ,mirror)))))

(defmethod port-mirror-width ((port graphic-forms-port) (sheet graphic-forms-graft))
  (graft-width sheet))

(defmethod port-mirror-height ((port graphic-forms-port) (sheet graphic-forms-graft))
  (graft-height sheet))

(defmethod graft ((port graphic-forms-port))
  (first (climi::port-grafts port)))

(defmethod port-allocate-pixmap ((port graphic-forms-port) sheet width height)
  ())

(defmethod port-deallocate-pixmap ((port graphic-forms-port) pixmap)
  #+nil
  (when (climi::port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))

(defmethod pointer-position ((pointer gfw-pointer))
  (values (slot-value pointer 'x) (slot-value pointer 'y)))

(defmethod pointer-button-state ((pointer gfw-pointer))
  ())

(defmethod port-modifier-state ((port graphic-forms-port))
  ())

(defmethod synthesize-pointer-motion-event ((pointer gfw-pointer))
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

(defmethod %set-port-keyboard-focus (focus (port graphic-forms-port) &key timestamp)
  (declare (ignore timestamp))
  ())

(defmethod port-force-output ((port graphic-forms-port))
  "Sent buffered request to graphic-forms-server, currently all request is not buffered so this is dummy."  
  ())

;; FIXME: What happens when CLIM code calls tracking-pointer recursively?
(defmethod port-grab-pointer ((port graphic-forms-port) pointer sheet)
  ())

(defmethod port-ungrab-pointer ((port graphic-forms-port) pointer sheet)
  ())

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


