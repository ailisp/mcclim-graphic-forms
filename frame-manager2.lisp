(in-package :clim-graphic-forms)

(defclass graphic-forms-frame-manager (frame-manager)
  ((%mirror-map :reader mirror-map
		:initform '((climi::top-level-sheet-pane . graphic-forms-top-level-sheet-pane))
		:allocation :class))
  (:documentation "The portable look and feel frame manager on Windows. Only top level window is mirrored to Windows native top levelwindow, all gadgets are portable CLIM implementation created by draw-*"))

;;; TODO: add %mirror-map just as graphic-forms-frame-manager
(defclass graphic-forms-native-frame-manager (graphic-froms-frame-manager)
  ()
  (:documentation "The Windows native look and feel frame manager. All CLIM gadgets are mirrored to Windows native ones."))

(defmethod make-pane-1 ((fm graphic-forms-frame-manager) (frame application-frame) type &rest args)
  (debug-print "make-pane-1" "origin" type "map to" (%maybe-mirror fm (%find-concrete-pane-class type)))
  (apply #'make-instance
	 (%maybe-mirror fm (%find-concrete-pane-class type))
	 :frame frame
	 :manager fm
	 :port (port frame)
	 args))

(defun %find-concrete-pane-class (type)
  "Find concrete pane of the abstract pane TYPE"
  (when (get type 'climi::concrete-pane-class-name)
    (setf type (get type 'climi::concrete-pane-class-name)))
  (class-name
   (or (find-class
	(intern (concatenate 'string (symbol-name type) "-PANE") :climi)
	nil)
       (if (keywordp type)
	   (find-class (intern (symbol-name type) :climi))
	   (find-class type)))))

(defmethod %maybe-mirror ((fm graphic-forms-frame-manager) class-name)
  (let ((sheet-and-mirror (assoc class-name (mirror-map fm))))
    (if sheet-and-mirror
	(find-class (cdr sheet-and-mirror))
	(find-class class-name))))

;; TODO: adopt-frame note-space-requirements-changed
