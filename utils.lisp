(in-package :clim-graphic-forms)

(defun requirement->size (req)
  (gfs:make-size :width (floor (space-requirement-width req))
		 :height (floor (space-requirement-height req))))

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

;; Copy from CLX
(declaim (inline round-coordinate))
(defun round-coordinate (x)
  "Function used for rounding coordinates."
  ;; We use "mercantile rounding", instead of the CL round to nearest
  ;; even number, when in doubt.
  ;;
  ;; Reason: As the CLIM drawing model is specified, you quite often
  ;; want to operate with coordinates, which are multiples of 1/2. 
  ;; Using CL:ROUND gives you "random" results. Using "mercantile
  ;; rounding" gives you consistent results.
  ;;
  ;; Note that CLIM defines pixel coordinates to be at the corners,
  ;; while in X11 they are at the centers. We don't do much about the
  ;; discrepancy, but rounding up at half pixel boundaries seems to
  ;; work well.
  (floor (+ x .5)))


