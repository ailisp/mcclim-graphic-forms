(in-package :clim-graphic-forms)

;;; note that the %mirror-map of this non-native frame manager also includes lots of gadget classes,
;;; because the push-button-pane must be a standard-full-mirrored-sheet-mixin's subclass to be drawn
;;; correctly
(defclass graphic-forms-frame-manager (frame-manager)
  ((%mirror-map :reader mirror-map
		:initform *graphic-forms-pane-class-map*
		:allocation :class))
  (:documentation "The portable look and feel frame manager on Windows. Only top level window is mirrored to Windows native top levelwindow, all gadgets are portable CLIM implementation created by draw-*"))

(defclass graphic-forms-native-frame-manager (graphic-froms-frame-manager)
  ((%mirror-map :initform *graphic-forms-native-pane-class-map*))
  (:documentation "The Windows native look and feel frame manager. All CLIM gadgets are mirrored to Windows native ones."))

(defmethod make-pane-1 ((fm graphic-forms-frame-manager) (frame application-frame) type &rest args)
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
