(in-package :clim-graphic-forms)

(defvar *graphic-forms-server* nil)

(defvar *graphic-forms-server-command-queue*
  (lparallel.queue:make-queue))

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

(defun close-graphic-forms-server ()
  (when *graphic-forms-server*
    (bt:destroy-thread *graphic-forms-server*)
    (setf *graphic-forms-server* nil)
    (loop
       (if (lparallel.queue:queue-empty-p *graphic-forms-server-command-queue*)
	   (return)
	   (lparallel.queue:pop-queue *graphic-forms-server-command-queue*)))))

(defun restart-graphic-forms-server ()
  (close-graphic-forms-server)
  (start-graphic-forms-server))

(defun graphic-forms-server-loop ()
  (cffi:with-foreign-object (msg-ptr '(:struct gfs::msg))
    (loop
       (let ((gm (gfs::peek-message msg-ptr (cffi:null-pointer) 0 0 (logior gfs::+pm-noyield+ gfs::+pm-remove+)))
	     (command-and-promise (graphic-forms-server-listen)))
	 (when command-and-promise
	   (debug-print "Server receive command:" (car command-and-promise))
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
  (debug-print (bt:current-thread))
  (case (first command)
    (gfw:show
     (debug-print (second command))
     (gfw:show (second command) (third command))
     (debug-print "Server show command processed."))
    (otherwise (let ((result (eval command)))
		 (debug-print "Server processed command:" command "Result:" result)
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


