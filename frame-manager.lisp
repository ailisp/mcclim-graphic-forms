(in-package :clim-graphic-forms)

;;; note that the %mirror-map of this non-native frame manager also includes lots of gadget classes,
;;; because the push-button-pane must be a standard-full-mirrored-sheet-mixin's subclass to be drawn
;;; correctly
(defclass graphic-forms-frame-manager (frame-manager)
  ()
  (:documentation "The portable look and feel frame manager on Windows. Only top level window is mirrored to Windows native top levelwindow, all gadgets are portable CLIM implementation created by draw-*"))

(defclass graphic-forms-native-frame-manager (graphic-froms-frame-manager)
  ()
  (:documentation "The Windows native look and feel frame manager. All CLIM gadgets are mirrored to Windows native ones."))

(defmethod make-pane-1 ((fm graphic-forms-frame-manager) (frame application-frame) type &rest args)
  (apply #'make-instance
	 (%maybe-mirroring fm (%find-concrete-pane-class type))
	 :frame frame
	 :manager fm
	 :port (port frame)
	 args))

(defun %find-concrete-pane-class (type)
  (if (or (eql (symbol-package type)
               (find-package '#:clim))
          (eql (symbol-package type)
               (find-package '#:climi))
          (eql (symbol-package type)
               (find-package '#:keyword))
	  (get type 'climi::concrete-pane-class-name))
      (%find-first-defined-class (%find-symbols (%generate-graphic-forms-pane-specs type)))
      type))

(defun %generate-graphic-forms-pane-specs (type)
  (append 
   `((:clim-gf #:graphic-forms- ,type #:-pane)
     (:clim-gf #:graphic-forms- ,type)
     (:climi #:graphic-forms- ,type #:-pane)
     (:climi #:graphic-forms- ,type))
   (%generate-standard-pane-specs type)))

(defun %generate-standard-pane-specs (type)
  (let ((mapping (get type 'climi::concrete-pane-class-name)))
    `((,(symbol-package mapping) ,mapping)
      (:climi ,mapping)
      (:climi ,type #:-pane)
      (:climi ,type))))

(defun %find-symbols (name-specs)
  (remove-if #'null (mapcar #'(lambda (x) (%find-symbol-from-spec (first x) (rest x))) name-specs)))

(defun %find-symbol-from-spec (package-spec name-components)
  (flet ((coerce-name-element (name-elt)
           (typecase name-elt
             (symbol (symbol-name name-elt))
             (sequence (coerce name-elt 'string))
             (t (princ-to-string name-elt)))))    
  (find-symbol
   (apply #'concatenate 'string (mapcar #'coerce-name-element name-components))
   package-spec)))

(defun %find-first-defined-class (types)
  (first
   (remove-if #'null 
              (mapcar (lambda (class-name)
                        (find-class class-name nil))
                      types))))

(defmethod %maybe-mirroring ((fm graphic-forms-frame-manager) concrete-pane-class)
  (when (and (not (subtypep concrete-pane-class 'mirrored-sheet-mixin))
	     (subtypep concrete-pane-class 'basic-pane))
    (let* ((concrete-pane-class-symbol (if (typep concrete-pane-class 'class)
					   (class-name concrete-pane-class)
					   concrete-pane-class))
	   (concrete-mirrored-pane-class (concatenate 'string
						      "GRAPHIC-FORMS-"
						      (symbol-name concrete-pane-class-symbol)))
	   (concrete-mirrored-pane-class-symbol (find-symbol concrete-mirrored-pane-class
							     :clim-gf)))
      (unless concrete-mirrored-pane-class-symbol
	(setf concrete-mirrored-pane-class-symbol
	      (intern concrete-mirrored-pane-class :clim-gf))
	(let ((superclasses (if (subtypep concrete-pane-class 'sheet-with-medium-mixin)
				(list 'graphic-forms-pane-mixin
				      concrete-pane-class-symbol)
				(list 'graphic-forms-pane-mixin 
				      'permanent-medium-sheet-output-mixin
				      concrete-pane-class-symbol))))
	  (eval
	   `(defclass ,concrete-mirrored-pane-class-symbol
		,superclasses
	      ()
	      (:metaclass ,(type-of (find-class concrete-pane-class-symbol)))))))
      (setf concrete-pane-class (find-class concrete-mirrored-pane-class-symbol))))
  concrete-pane-class)

;; TODO: adopt-frame note-space-requirements-changed
