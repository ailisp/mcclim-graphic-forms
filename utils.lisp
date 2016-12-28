(in-package :clim-graphic-forms)

(defun requirement->size (req)
  (<+ `(gfs:make-size :width ,(floor (space-requirement-width req))
		      :height ,(floor (space-requirement-height req)))))

;;; This function should only be called in graphics-forms-server thread
(defun translate-rectangle (gfw-rect)
  (let ((pnt (gfs:location gfw-rect))
        (size (gfs:size gfw-rect)))
    (make-rectangle* (gfs:point-x pnt)
                     (gfs:point-y pnt)
                     (+ (gfs:point-x pnt) (gfs:size-width size))
                     (+ (gfs:point-y pnt) (gfs:size-height size)))))

(declaim (inline coordinates->rectangle))
(defun coordinates->rectangle (left top right bottom)
  (<+ `(gfs:create-rectangle :x ,(floor left)
			     :y ,(floor top)
			     :width ,(floor (- right left))
			     :height ,(floor (- bottom top)))))

(defun coordinates->points (seq)
  (loop for i from 0 below (length seq) by 2
     collect (<+ `(gfs:make-point :x ,(floor (elt seq i))
				  :y ,(floor (elt seq (+ i 1)))))))

(declaim (inline radians->degrees))
(defun radians->degrees (rads)
  (floor (* rads 180) pi))

(defparameter *clim-concrete-pane-class*
  '(push-button-pane
    toggle-button-pane
    menu-button-pane
    scroll-bar-pane
    slider-pane
    radio-box-pane
    check-box-pane
    generic-list-pane
    generic-option-pane
    text-field-pane
    text-editor-pane))

(defun %make-pane-class-map (infix)
  (acons 'climi::top-level-sheet-pane 'graphic-forms-top-level-sheet-pane
	 (mapcar (lambda (clim-concrete-pane-class)
		   (cons clim-concrete-pane-class
			 (intern (concatenate 'string
					      infix
					      (symbol-name clim-concrete-pane-class)))))
		 *clim-concrete-pane-class*)))

(defparameter *graphic-forms-pane-class-map*
  (%make-pane-class-map "GRAPHIC-FORMS-"))

(defparameter *graphic-forms-native-pane-class-map*
  (%make-pane-class-map "GRAPHIC-FORMS-NATIVE-"))

(defun %expand-define-graphic-forms-pane-class (concrete-pane-class-name pane-map)
  `(defclass ,(cdr (assoc concrete-pane-class-name pane-map)) (standard-full-mirrored-sheet-mixin
		  ,concrete-pane-class-name)
     ()))
