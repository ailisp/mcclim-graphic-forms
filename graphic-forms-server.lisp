(in-package :clim-graphic-forms)

(defvar *graphic-forms-server* nil)

(defvar *graphic-forms-server-command-queue*
  (lparallel.queue:make-queue))

(defvar *graphic-forms-server-event-queue*
  (lparallel.queue:make-queue))

(defvar *graphic-forms-mouse-down-sheet* nil
  "Windows' mouse-up event is different with X's mouse-release event that the window mouse-up happens is the window
that user release the mouse, in X it's always the previous window mouse-press (mouse-down) event happens. So we need
to track this information manually.")

(defparameter *graphic-forms-server-debug* t)

;; This must be set, otherwise because thread bindings, directly print to *standard-output* will not display in SLIME.
(defparameter *graphic-forms-server-debug-output* *standard-output*)

(defparameter *debug-print-lock* (bt:make-recursive-lock "DEBUG-PRINT-LOCK"))

(defun debug-print(&rest args)
  (bt:with-recursive-lock-held (*debug-print-lock*)
    (when *graphic-forms-server-debug*
      (format *graphic-forms-server-debug-output* "~{~a~%~}" args))))

(defun debug-prin1 (&rest args)
  (bt:with-recursive-lock-held (*debug-print-lock*)
    (when *graphic-forms-server-debug*
      (format *graphic-forms-server-debug-output* "~{~a ~}~%" args))))

(defun start-graphic-forms-server ()
  (or *graphic-forms-server*
      (setf *graphic-forms-server*
	    (bt:make-thread 'graphic-forms-server-loop :name "GRAPHIC-FORMS-SERVER"))))

(defun empty-queue (queue)
  (loop
     (if (lparallel.queue:queue-empty-p queue)
	 (return)
	 (lparallel.queue:pop-queue queue))))

(defun close-graphic-forms-server ()
  (when *graphic-forms-server*
    (bt:destroy-thread *graphic-forms-server*)
    (setf *graphic-forms-server* nil)
    (empty-queue *graphic-forms-server-command-queue*)
    (empty-queue *graphic-forms-server-event-queue*)))

(defun restart-graphic-forms-server ()
  (close-graphic-forms-server)
  (start-graphic-forms-server))

(defun graphic-forms-server-loop ()
  (cffi:with-foreign-object (msg-ptr '(:struct gfs::msg))
    (loop
       (let ((gm (gfs::peek-message msg-ptr (cffi:null-pointer) 0 0 (logior gfs::+pm-noyield+ gfs::+pm-remove+)))
	     (command-and-promise (graphic-forms-server-listen)))
	 (when command-and-promise
	   (graphic-forms-server-read)
	   (lparallel:fulfill (cdr command-and-promise)
	     (process-command (car command-and-promise))))
	 (when (/= gm 0)
	  (cffi:with-foreign-slots ((gfs::message gfs::wparam) msg-ptr (:struct gfs::msg))
	    (when (funcall 'gfw:default-message-filter gm msg-ptr)
	      (return-from graphic-forms-server-loop gfs::wparam))))))))

(defun graphic-forms-server-listen ()
  (lparallel.queue:peek-queue *graphic-forms-server-command-queue*))

(defun graphic-forms-server-read ()
  (lparallel.queue:pop-queue *graphic-forms-server-command-queue*))

(defun send-to-graphic-forms-server (command)
  (assert *graphic-forms-server*)
  (let ((p (lparallel:promise)))
    (lparallel.queue:push-queue (cons command p) *graphic-forms-server-command-queue*)
    p))

(defun send-to-graphic-forms-server/blocked (command)
  (lparallel:force (send-to-graphic-forms-server command)))

(defmacro <- (command)
  `(send-to-graphic-forms-server ,command))

(defmacro <+ (command)
  `(send-to-graphic-forms-server/blocked ,command))

;;; TODO: need to examine command and recover from error
;;; There must be no blocked command send in process-command
(defun process-command (command)
  (case (first command)
    (gfw:show
     (gfw:show (second command) (third command)))
    (otherwise (let ((result (eval command)))
		 result))))

;;; This return should be free via gfs:dispose
(defun make-graphics-context (&optional thing)
  (cond
    ((null thing)
     (make-instance 'gfg:graphics-context))
    ((typep thing 'gfw:widget)
     (make-instance 'gfg:graphics-context :widget thing))
    ((typep thing 'gfg:image)
     (make-instance 'gfg:graphics-context :image thing))
    (t
     (error 'gfs:toolkit-error
	    :detail (format nil "~a is an unsupported type" thing)))))

(defmacro with-server-graphics-context ((gc &optional thing) &body body)
  `(let ((,gc (<+ `(make-graphics-context ,,thing))))
     (unwind-protect
	  (progn
	    ,@body)
       (<- `(gfs:dispose ,,gc)))))

(defmacro with-server-root-window ((win) &body body)
  `(let ((,win (<+ `(make-instance 'gfw:root-window))))
     (unwind-protect
	  (progn
	    ,@body)
       (<- `(gfs:dispose ,,win)))))

(start-graphic-forms-server)

(defun server-add-event (event)
  (setf (slot-value event 'climi::timestamp) (gfw:obtain-event-time))
  (lparallel.queue:push-queue event *graphic-forms-server-event-queue*))

(defun server-get-event ()
  (lparallel.queue:pop-queue *graphic-forms-server-event-queue*))

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

(defclass gfw-window (gfw:window gf-mirror-mixin) ()
  (:documentation "Server side window for unmanaged-top-level-sheet-panes"))

(defclass sheet-event-dispatcher (gfw:event-dispatcher)
  ((port
    :accessor port
    :initform nil))
  (:documentation "Server side windows native event handler"))

(defvar *sheet-dispatcher* (<+ `(make-instance 'sheet-event-dispatcher)))

;;;
;;; dispatchers and callbacks
;;;

(defmethod gfw:event-close ((self sheet-event-dispatcher) mirror)
  (server-add-event
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
;               (not (image-of (sheet-medium sheet)))
	       )
      (let ((c (ink-to-color (sheet-medium sheet)
                             (sheet-desired-ink sheet))))
        (setf (gfg:background-color gc) c
              (gfg:foreground-color gc) c))
      (gfg:draw-filled-rectangle gc rect))
    (server-add-event
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
  (server-add-event
   (generate-configuration-event mirror (gfw:location mirror) size)))

(defmethod gfw:event-move ((self sheet-event-dispatcher) mirror pnt)
  (server-add-event
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
  (server-add-event 
   (make-instance 'pointer-motion-event
		  :pointer 0
		  :sheet (sheet mirror)
		  :x (gfs:point-x point)
		  :y (gfs:point-y point)
		  :button (translate-button-name button)
		  ;; FIXME:
;;; 		       :graft-x
;;; 		       :graft-y
		  :modifier-state 0)))

(defmethod gfw:event-mouse-down ((self sheet-event-dispatcher) mirror point button)
  (setf *graphic-forms-mouse-down-sheet* (sheet mirror))
  (server-add-event 
   (make-instance 'pointer-button-press-event
		  :pointer 0
		  :sheet *graphic-forms-mouse-down-sheet*
		  :x (gfs:point-x point)
		  :y (gfs:point-y point)
		  :button (translate-button-name button)
		  ;; FIXME:
;;; 		       :graft-x
;;; 		       :graft-y
		  :modifier-state 0)))

(defmethod gfw:event-mouse-up ((self sheet-event-dispatcher) mirror point button)
  (server-add-event 
   (make-instance 'pointer-button-release-event
		  :pointer 0
		  :sheet *graphic-forms-mouse-down-sheet*
		  :x (gfs:point-x point)
		  :y (gfs:point-y point)
		  :button (translate-button-name button)
		  ;; FIXME:
;;; 		       :graft-x
;;; 		       :graft-y
		  :modifier-state 0))
  (setf *graphic-forms-mouse-down-sheet* nil))

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
  (server-add-event 
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
  (server-add-event
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

(defmethod gfw:event-mouse-enter ((self sheet-event-dispatcher) mirror point button)
  (server-add-event
   (make-instance 'pointer-enter-event
		  :pointer 0
		  :sheet (sheet mirror)
		  :x (gfs:point-x point)
		  :y (gfs:point-y point)
		  :button (translate-button-name button)
		  :modifier-state 0)))

(defmethod gfw:event-mouse-exit ((self sheet-event-dispatcher) mirror point button)
  (server-add-event
   (make-instance 'pointer-exit-event
		  :pointer 0
		  :sheet (sheet mirror)
		  :x (gfs:point-x point)
		  :y (gfs:point-y point)
		  :button (translate-button-name button)
		  :modifier-state 0)))
