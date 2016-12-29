(in-package :clim-graphic-forms)

;;; This represents server side object
;;; TODO: move to server.lisp
(defclass gf-mirror-mixin ()
  ((sheet
    :accessor sheet
    :initarg :sheet
    :initform nil))
  (:documentation "Server side graphic-forms object"))

(defclass gfw-top-level (gfw:top-level gf-mirror-mixin) ()
  (:documentation "Server side top level window"))
(defclass gfw-panel (gfw:panel gf-mirror-mixin) ()
  (:documentation "Server side gadgets"))

(defclass sheet-event-dispatcher (gfw:event-dispatcher)
  ((port
    :accessor port
    :initform nil))
  (:documentation "Server side windows native event handler"))

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

;;; This function should only be called in graphic-forms-server thread
(defun enqueue (port event)
  (setf (slot-value event 'climi::timestamp) (gfw:obtain-event-time))
;  (push event (events port))
  )

(defvar *sheet-dispatcher* (<+ `(make-instance 'sheet-event-dispatcher)))

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
    	   :name (format nil "~S's event process." port))))
  (debug-print "initialize-instance graphic-forms port finished"))

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

(defmethod port-set-mirror-transformation ((port graphic-forms-port) (mirror gfw-top-level) transformation)
  ;; FIXME: does McCLIM really need to set position of top-level window's?
  ())

(defmethod port-set-mirror-transformation ((port graphic-forms-port) (mirror gf-mirror-mixin) transformation)
  (multiple-value-bind (x y)
      (transform-position transformation 0 0)
    (<+ `(setf (gfw:location ,mirror)
	       (gfs:make-point :x ,(floor x)
			       :y ,(floor y))))))

(defclass graphic-forms-top-level-sheet-pane (standard-full-mirrored-sheet-mixin
					      permanent-medium-sheet-output-mixin
					      climi::top-level-sheet-pane)
  ())

;;;
;;; sheet methods
;;;

(defmethod realize-mirror ((port graphic-forms-port) (sheet graphic-forms-top-level-sheet-pane))
  (debug-prin1 "realize-mirror called on: " sheet)
  (let* ((mirror (<+ `(make-instance 'gfw-top-level
				     :sheet ,sheet
				     :dispatcher ,*sheet-dispatcher*
				     :style '(:workspace)
				     :text ,(frame-pretty-name (pane-frame sheet))))))
    (climi::port-register-mirror (port sheet) sheet mirror)
    mirror))

(defmethod realize-mirror ((port graphic-forms-port) (sheet standard-full-mirrored-sheet-mixin))
  (let* ((parent (sheet-mirror (sheet-parent sheet)))
         (mirror (<+ `(make-instance 'gfw-panel
				     :sheet ,sheet
				     :dispatcher ,*sheet-dispatcher*
				     :style '() ;was: '(:border)
				     :parent ,parent))))
    (climi::port-register-mirror (port sheet) sheet mirror)
    mirror))

(defmethod destroy-mirror ((port graphic-forms-port) (sheet standard-full-mirrored-sheet-mixin))
  (let ((mirror (climi::port-lookup-mirror port sheet)))
    (climi::port-unregister-mirror port sheet mirror)
    (<- `(gfs:dispose ,mirror))))

(defmethod port-enable-sheet ((port graphic-forms-port) (sheet standard-full-mirrored-sheet-mixin))
  (<- `(gfw:show ,(climi::port-lookup-mirror port sheet) t)))

(defmethod port-disable-sheet ((port graphic-forms-port) (sheet standard-full-mirrored-sheet-mixin))
  (<- `(gfw:show ,(climi::port-lookup-mirror port sheet) nil)))

(defmethod destroy-port :before ((port graphic-forms-port))
  ())

(defmethod port-motion-hints ((port graphic-forms-port) (sheet standard-full-mirrored-sheet-mixin))
  ())

(defmethod (setf port-motion-hints)
    (value (port graphic-forms-port) (sheet standard-full-mirrored-sheet-mixin))
  value)

(defmethod get-next-event ((port graphic-forms-port) &key wait-function (timeout nil))
  ())

(defmethod process-next-event :after ((port graphic-forms-port) &key wait-function (timeout nil))
  (declare (ignore wait-function timeout))
  (render-pending-mediums))

(defmethod make-graft ((port graphic-forms-port) &key (orientation :default) (units :device))
  (let ((result (make-instance 'graphic-forms-graft
			       :port port :mirror (gensym)
			       :orientation orientation :units units)))
    (debug-prin1 "make-graft" result (slot-value result 'climi::mirror))
    result))

(defmethod make-medium ((port graphic-forms-port) sheet)
  #+nil (gfs::debug-format "creating medium for ~a~%" (class-of sheet))
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

(defmethod port-mirror-width ((port graphic-forms-port) (sheet standard-full-mirrored-sheet-mixin))
  #+nil (<- `(gfs::debug-format "port-mirror-width called for ~a~%" ,sheet))
  (let ((mirror (climi::port-lookup-mirror port sheet)))
    (<+ `(gfs:size-width (gfw:size ,mirror)))))

(defmethod port-mirror-height ((port graphic-forms-port) (sheet standard-full-mirrored-sheet-mixin))
  #+nil (<- `(gfs::debug-format "port-mirror-height called for ~a~%" ,sheet))
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


;;;
;;; dispatchers and callbacks
;;;

(defmethod gfw:event-close ((self sheet-event-dispatcher) mirror)
  (enqueue (port self)
	   (make-instance 'window-manager-delete-event :sheet (sheet mirror))))

;; copy&paste from port.lisp|CLX:
(defun sheet-desired-ink (sheet)
  (typecase sheet
    (sheet-with-medium-mixin
      (medium-background sheet))
    (basic-pane
      ;; CHECKME [is this sensible?] seems to be
      (let ((background (pane-background sheet)))
	(if (typep background 'color)
	    background
	    +white+)))
    (t
     +white+)))

;;; This function should only be called in graphics-forms-server thread
(defun ink-to-color (medium ink)
  (debug-print "blbobobobobo")
  (cond
    ((subtypep (class-of ink) (find-class 'climi::opacity))
     (setf ink (medium-foreground medium))) ; see discussion of opacity in design.lisp
    ((eql ink +foreground-ink+)
     (setf ink (medium-foreground medium)))
    ((eql ink +background-ink+)
     (setf ink (medium-background medium)))
    ((eql ink +flipping-ink+)
     (warn "+flipping-ink+ encountered in ink-to-color~%")
     (setf ink nil)))
  (debug-prin1 "kkkkkkkkk")
  (if ink
      (multiple-value-bind (red green blue) (clim:color-rgb ink)
	(gfg:make-color :red (min (truncate (* red 256)) 255)
			:green (min (truncate (* green 256)) 255)
			:blue (min (truncate (* blue 256)) 255)))
      (progn (debug-prin1 "nononono")
       (with-server-graphics-context (gc (target-of medium))
	 (gfg:background-color gc)))))

(defmethod gfw:event-paint ((self sheet-event-dispatcher) mirror gc rect)
  (let ((sheet (sheet mirror)))
    (when (and (typep sheet 'sheet-with-medium-mixin)
               (not (image-of (sheet-medium sheet))))
      (debug-print "~~~~~~~~~~~")
      (debug-print(ink-to-color (sheet-medium sheet) (sheet-desired-ink sheet)))
      (let ((c (ink-to-color (sheet-medium sheet)
                             (sheet-desired-ink sheet))))
        (setf (gfg:background-color gc) c
              (gfg:foreground-color gc) c))
      (gfg:draw-filled-rectangle gc rect))
    (enqueue (port self)
             (make-instance 'window-repaint-event
                            :sheet sheet
                            :region (translate-rectangle rect)))))

;;; This function should only be called in graphics-forms-server thread
(defun generate-configuration-event (mirror pnt size)
  (make-instance 'window-configuration-event
                 :sheet (sheet mirror)
                 :x (gfs:point-x pnt)
                 :y (gfs:point-y pnt)
                 :width (gfs:size-width size)
                 :height (gfs:size-height size)))

(defmethod gfw:event-resize ((self sheet-event-dispatcher) mirror size type)
  (declare (ignore type))
  (setf size (gfw:client-size mirror))
  (let ((sheet (sheet mirror)))
    (if (and sheet (subtypep (class-of sheet) 'sheet-with-medium-mixin))
        (let ((medium (climi::sheet-medium sheet)))
          (when (and medium (image-of medium))
            (resize-medium-buffer medium size)))))
  (enqueue (port self)
           (generate-configuration-event mirror (gfw:location mirror) size)))

(defmethod gfw:event-move ((self sheet-event-dispatcher) mirror pnt)
  (enqueue (port self)
           (generate-configuration-event mirror pnt (gfw:client-size mirror))))

(defun translate-button-name (name)
  (case name
    (:left-button +pointer-left-button+)
    (:right-button +pointer-right-button+)
    (:middle-button +pointer-middle-button+)
    (t
     (warn "unknown button name: ~A" name)
     nil)))

(defmethod gfw:event-mouse-move
    ((self sheet-event-dispatcher) mirror point button)
  (enqueue (port self)
	   (make-instance 'pointer-motion-event
			  :pointer 0
			  :sheet (sheet mirror)
			  :x (gfs:point-x point)
			  :y (gfs:point-y point)
			  :button (translate-button-name button)
			  ;; FIXME:
;;; 		       :graft-x
;;; 		       :graft-y
			  :modifier-state 0
			  )))

(defmethod gfw:event-mouse-down ((self sheet-event-dispatcher) mirror point button)
  (debug-prin1 "mouse-down event")
  (enqueue (port self)
	   (make-instance 'pointer-button-press-event
			  :pointer 0
			  :sheet (sheet mirror)
			  :x (gfs:point-x point)
			  :y (gfs:point-y point)
			  :button (translate-button-name button)
			  ;; FIXME:
;;; 		       :graft-x
;;; 		       :graft-y
			  :modifier-state 0
			  )))

(defmethod gfw:event-mouse-up ((self sheet-event-dispatcher) mirror point button)
  (enqueue (port self)
	   (make-instance 'pointer-button-release-event
			  :pointer 0
			  :sheet (sheet mirror)
			  :x (gfs:point-x point)
			  :y (gfs:point-y point)
			  :button (translate-button-name button)
			  ;; FIXME:
;;; 		       :graft-x
;;; 		       :graft-y
			  :modifier-state 0
			  )))

(defun char-to-sym (char)
  (case char
    (#\  :| |) (#\! :!) (#\" :|"|) (#\# :|#|) (#\$ :$) (#\% :%) (#\& :&)
    (#\' :|'|) (#\( :|(|) (#\) :|)|) (#\* :*) (#\+ :+) (#\, :|,|) (#\- :-)
    (#\. :|.|) (#\/ :/) (#\0 :|0|) (#\1 :|1|) (#\2 :|2|) (#\3 :|3|) (#\4 :|4|)
    (#\5 :|5|) (#\6 :|6|) (#\7 :|7|) (#\8 :|8|) (#\9 :|9|) (#\: :|:|) (#\; :|;|)
    (#\< :<) (#\= :=) (#\> :>) (#\? :?) (#\@ :@) (#\A :A) (#\B :B) (#\C :C)
    (#\D :D) (#\E :E) (#\F :F) (#\G :G) (#\H :H) (#\I :I) (#\J :J) (#\K :K)
    (#\L :L) (#\M :M) (#\N :N) (#\O :O) (#\P :P) (#\Q :Q) (#\R :R) (#\S :S)
    (#\T :T) (#\U :U) (#\V :V) (#\W :W) (#\X :X) (#\Y :Y) (#\Z :Z) (#\[ :[)
    (#\\ :|\\|) (#\] :]) (#\_ :_) (#\` :|`|) (#\a :|a|) (#\b :|b|) (#\c :|c|)
    (#\d :|d|) (#\e :|e|) (#\f :|f|) (#\g :|g|) (#\h :|h|) (#\i :|i|) (#\j :|j|)
    (#\k :|k|) (#\l :|l|) (#\m :|m|) (#\n :|n|) (#\o :|o|) (#\p :|p|) (#\q :|q|)
    (#\r :|r|) (#\s :|s|) (#\t :|t|) (#\u :|u|) (#\v :|v|) (#\w :|w|) (#\x :|x|)
    (#\y :|y|) (#\z :|z|) (#\{ :{) (#\| :|\||) (#\} :}) (#\Backspace :BACKSPACE)
    (#\Tab :TAB) (#\Return :RETURN) (#\Rubout :DELETE)))

(defmethod gfw:event-key-down ((self sheet-event-dispatcher) mirror code char)
  (enqueue (port self)
	   (make-instance 'key-press-event
			  :key-name (char-to-sym char)
			  :key-character char
			  :sheet (sheet mirror)
			  ;; FIXME:
			  :x 0
			  :y 0
			  :modifier-state 0
;;; 			 :graft-x root-x
;;; 			 :graft-y root-y
			  )))

(defmethod gfw:event-key-up ((self sheet-event-dispatcher) mirror code char)
  (enqueue (port self)
	   (make-instance 'key-release-event
			  :key-name (char-to-sym char)
			  :key-character char
			  :sheet (sheet mirror)
			  ;; FIXME:
			  :x 0
			  :y 0
			  :modifier-state 0
;;; 			 :graft-x root-x
;;; 			 :graft-y root-y
			  )))

