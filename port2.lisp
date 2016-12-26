(in-package :clim-graphic-forms)

(defclass gf-port (basic-port standard-event-port-mixin)
  ((screen :initform nil :accessor gf-port-screen)
   (events :initform nil :accessor gf-port-events)))

(defmethod initialize-instance :after ((port gf-port) &rest args)
  (start-graphic-forms-server)
  (setf events (lparallel.queue:make-queue))
  (setf (gf-port-screen port) (<+ `(make-instance 'gfw::root-window))))

(defmethod destroy-port :after ((port gf-port))
  (close-graphic-forms-server)
  (<- `(gfs:dispose ,(gf-port-screen port)))
  (setf (gf-port-screen port) nil)
  (setf events nil))

(defmethod get-next-event ((port gf-port) &key wait-function (timeout nil))
  (declare (ignore wait-function timeout)) ; FIXME
  (lparallel.queue:pop-queue (gf-port-events port)))

(defmethod make-graft ((port gf-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'gf-graft
			      :port port
			      :mirror (gf-port-screen port)
			      :orientation orientation :units units)))
    (setf (sheet-region graft)
	  (make-bounding-rectangle 0 0
				   (graft-width graft)
				   (graft-height height)))
    (push graft (climi::port-grafts port))))

(defmethod port-allocate-pixmap ((port gf-port) sheet width height)
  (let ((pixmap (make-instance 'mirrored-pixmap
			       :sheet sheet
			       :width width
			       :height height
			       :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port gf-port) pixmap)
  (when (climi::port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))

(defmethod port-frame-keyboard-input-focus
    ((port gf-port) frame)
  (frame-properties frame 'focus))

(defmethod (setf port-frame-keyboard-input-focus)
    (focus (port gf-port) frame)
  (<+ `(gfw:give-focus ,(sheet-mirror focus)))
  (setf (frame-properties frame 'focus) focus))

;;; Windows' event model is different from X that, on windows, event is processed per application (globally),
;;; but not dispatch to clients (except button and menu). So we only need to set what is grabbed and in our
;;; manual event dispatch, check whether grabbed and use different dispatch strategy.
;;; in order to make grab and ungrab work correctly in recursive tracking-pointer, we use push and pop here.
(defmethod port-grab-pointer ((port gf-port) pointer sheet)
  (declare (ignore pointer))
  (push sheet (pointer-grab-sheet port)))

(defmethod port-ungrab-pointer ((port gf-port) pointer sheet)
  (declare (ignore sheet pointer))
  (pop (pointer-grab-sheet port)))

(defclass gf-mirror-mixin ()
  ((sheet
    :accessor sheet
    :initarg :sheet
    :initform nil)))

(defclass gfw-top-level (gfw:top-level gf-mirror-mixin) ())

;;; TODO: change the cursor pic when move onto the sheet
(defmethod set-sheet-pointer-cursor ((port graphic-forms-port) sheet cursor)
  ())

(defclass gf-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)))

;;; TODO: Create a CLIM pointer motion event based on the current pointer state
(defmethod synthesize-pointer-motion-event ((pointer gf-pointer))
  (let* ((port (port pointer))
	 (sheet (port-pointer-sheet port)))
    (when sheet
      (let ((mirror (sheet-direct-mirror sheet)))
	(when mirror
	  (multiple-value-bind (x y same-screen-p child mask root-x root-y)
	      (xlib:query-pointer mirror)
	    (declare (ignore child))
	    (when same-screen-p
	      (make-instance
	       'pointer-motion-event
	       :pointer 0 :button (button-from-state mask)
	       :x x :y y
	       :graft-x root-x
	       :graft-y root-y
	       :sheet sheet
	       :modifier-state (clim-xcommon:x-event-state-modifiers port mask)
	       ;; The event initialization code will give us a
	       ;; reasonable timestamp.
	       :timestamp 0))))))))

(defmethod allocate-space :after ((pane climi::top-level-sheet-pane) width height)
  ())

(defmethod destroy-mirror ((port graphic-forms-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (climi::port-lookup-mirror port sheet)))
    (climi::port-unregister-mirror port sheet mirror)
    (<- `(gfs:dispose ,mirror))))

(defmethod destroy-mirror ((port graphic-forms-port) (pixmap pixmap))
  ;; TODO
  ())

(defmethod graft ((port graphic-forms-port))
  (first (climi::port-grafts port)))

(defmethod make-medium ((port graphic-forms-port) sheet)
  (make-instance 'graphic-forms-medium :sheet sheet))

;; TODO (values pointer x pointer y) 
(defmethod pointer-position ((pointer gf-pointer))
  ())

;;; TODO setf pointer-position
