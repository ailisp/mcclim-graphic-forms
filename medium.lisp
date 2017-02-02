(in-package :clim-graphic-forms)

(defclass graphic-forms-medium (basic-medium)
  ((font
    :accessor font-of
    :initform nil)
   (image
    :accessor image-of
    :initform nil)
   ;;; Copy from CLX
   (clipping-region-tmp
    :initform (vector 0 0 0 0)
    :documentation "This object is reused to avoid consing in the
 most common case when configuring the clipping region.")))

(defvar *medium-origin*     (<+ `(gfs:make-point)))
(defvar *mediums-to-render* nil)

(defun add-medium-to-render (medium)
  (when (image-of medium)
    (pushnew medium *mediums-to-render* :test #'eql)))

(defun remove-medium-to-render (medium)
  (setf *mediums-to-render* (remove medium *mediums-to-render*)))

(defun render-medium-buffer (medium)
  (let ((mirror (climi::port-lookup-mirror (port (medium-sheet medium)) (medium-sheet medium))))
    (with-server-graphics-context (gc mirror)
      (<+ `(gfg:draw-image ,gc ,(image-of medium) ,*medium-origin*)))))

(defun render-pending-mediums ()
  (loop for medium in *mediums-to-render*
        do (render-medium-buffer medium))
  (setf *mediums-to-render* nil))

(defun %target-of (medium)
  (let ((sheet (medium-sheet medium)))
    (or (image-of medium)
	(let* ((region (sheet-region sheet))
	       (width (floor (bounding-rectangle-max-x region)))
	       (height (floor (bounding-rectangle-max-y region))))
	  (setf (image-of medium)
		(<+ `(make-instance 'gfg:image
				    :size (gfs:make-size :width ,width :height ,height))))))))

(defun %set-gc-foreground-background (medium gc ink)
  (if (eql ink +flipping-ink+)
      (progn
	(<+ `(gfg::set-mix-mode ,gc gfg::+r2-xorpen+)))
      (let ((color (ink-to-color medium ink)))
	(<+ `(setf (gfg:background-color ,gc) ,color 
		   (gfg:foreground-color ,gc) ,color)))))

(defmacro with-graphic-forms-medium ((gc medium &key (tr 'tr) ink) &body body)
  (let ((gink (gensym "ink")))
   `(when (%target-of ,medium)
      (with-server-graphics-context (,gc (%target-of ,medium))
	(%set-gc-clipping-region ,medium ,gc)
	(let ((,gink (or ,ink (medium-ink ,medium))))
	  (unless (eql ,gink +transparent-ink+)
	    (%set-gc-foreground-background ,medium ,gc ,gink)
	    (let ((,tr (sheet-native-transformation (medium-sheet ,medium))))
	      ,@body)
	    (add-medium-to-render ,medium)))))))

;;; This function should only be called in graphic-forms-server thread
(defun resize-medium-buffer (medium size)
  (let ((old-image (image-of medium)))
    (when old-image
      (if (not (gfs:disposed-p old-image))
        (let ((old-size (gfg:size old-image)))
          (unless (gfs:equal-size-p size old-size)
            (gfs:dispose old-image)
            (setf old-image nil)))
        (setf old-image nil)))
    (unless old-image
      (setf (image-of medium) (make-instance 'gfg:image :size size)))))

(defun destroy-medium (medium)
  (remove-medium-to-render medium)
  (let ((image (image-of medium)))
    (if (and image (not (<+ `(gfs:disposed-p ,image))))
      (<+ `(gfs:dispose ,image))))
  (let ((font (font-of medium)))
    (if (and font (not (<+ `(gfs:disposed-p ,font))))
      (<+ `(gfs:dispose ,font)))
    (setf (font-of medium) nil)))


;;; CLIM text-style and Windows native font object mapping
(defun update-medium-font (medium text-style)
  (with-server-graphics-context
      (gc (climi::port-lookup-mirror (port (medium-sheet medium)) (medium-sheet medium)))
    (let* ((old-data
	    (when (font-of medium)
	      (<+ `(gfg:data-object ,(font-of medium) ,gc))))
	   (new-font (text-style-to-font gc text-style old-data))) 
      (when new-font
	(when old-data
	  (<+ `(gfs:dispose ,(font-of medium)))
	  (setf (font-of medium) nil))
	(setf (font-of medium) new-font)))))

(defun normalize-text-data (text)
  (etypecase text
    (string    text)
    (character (string text))
    (symbol    (symbol-name text))))

(defparameter *text-style-family-to-font-family-map*
  '((:fix . "Lucida Console")
    (:fixed . "Lucida Console")
    (:serif . "Times New Roman")
    (:sans-serif . "Arial")))

(defun text-style-family-to-font-family (family)
  (if (stringp family)
      family
      (or (cdr (assoc family *text-style-family-to-font-family-map*))
	  (error "Can't find font family for face name ~s.~%" family))))

(defparameter *text-style-size-to-font-size-map*
  '((:tiny       .  6)
    (:very-small .  7)
    (:small      .  8)
    (:normal     . 10)
    (:large      . 12)
    (:very-large . 14)
    (:huge       . 16)))

(defparameter *default-font-size* 10)

(defun text-style-size-to-font-size (size)
  (or (cdr (assoc size *text-style-size-to-font-size-map*))
      *default-font-size*))

;;family can decide fixed or normal, in CLIM is belongs to face
(defun text-style-face-to-font-face (face family) 
  (let ((style nil))
    (pushnew (case face
	       ((:bold :bold-italic :bold-oblique :italic-bold :oblique-bold)
		:bold)
	       (otherwise
		:normal))
	     style)
    (pushnew (case face
	       ((:bold-italic :italic :italic-bold)
		:italic)
	       (otherwise
		:normal))
	     style)
    (pushnew (case family
	       ((:fix :fixed) :fixed)
	       (otherwise     :normal))
	     style)
    style))

(defun text-style-to-font (gc text-style old-data)
  (multiple-value-bind (family face size)
      (text-style-components (merge-text-styles text-style *default-text-style*))
    ;; FIXME: what to do about font data char sets?
    (let ((face-name (text-style-family-to-font-family family))
          (pnt-size  (text-style-size-to-font-size size))
          (style     (text-style-face-to-font-face face family)))
      (if (or (null old-data)
              (not (eql pnt-size (<+ `(gfg:font-data-point-size ,old-data))))
              (string-not-equal face-name (<+ `(gfg:font-data-face-name ,old-data)))
              (/= (length style)
                  (length (intersection style (<+ `(gfg:font-data-style ,old-data))))))
          (let ((new-data (<+ `(gfg:make-font-data :face-name ,face-name
						   :point-size ,pnt-size
						   :style ',style))))
            (<+ `(make-instance 'gfg:font :gc ,gc :data ,new-data)))
          (<+ `(make-instance 'gfg:font :gc ,gc :data ,old-data))))))

(defmethod (setf medium-text-style) :before (text-style (medium graphic-forms-medium))
  (update-medium-font medium (merge-text-styles text-style
						(medium-default-text-style medium))))

(defmethod (setf medium-line-style) :before (line-style (medium graphic-forms-medium))
  ())

(defmethod medium-draw-point* ((medium graphic-forms-medium) x y)
  (with-graphic-forms-medium (gc medium)
    (climi::with-transformed-position (tr x y)
      (<+ `(gfg:draw-point ,gc (gfs:make-point :x ,(floor x)
					       :y ,(floor y)))))))

(defmethod medium-draw-points* ((medium graphic-forms-medium) coord-seq)
  (with-graphic-forms-medium (gc medium)
    (loop for (x y) on (coerce coord-seq 'list) by #'cddr do
	 (climi::with-transformed-position (tr x y)
	   (<+ `(gfg:draw-point ,gc
				(gfs:make-point :x ,(floor x)
						:y ,(floor y))))))))

(defmethod medium-draw-line* ((medium graphic-forms-medium) x1 y1 x2 y2)
  (with-graphic-forms-medium (gc medium)
    (climi::with-transformed-position (tr x1 y1)
      (climi::with-transformed-position (tr x2 y2)
	(<+ `(gfg:draw-line ,gc
			    (gfs:make-point :x ,(floor x1)
					    :y ,(floor y1))
			    (gfs:make-point :x ,(floor x2)
					    :y ,(floor y2))))))))

(defmethod medium-draw-lines* ((medium graphic-forms-medium) coord-seq)
  (with-graphic-forms-medium (gc medium)
    (loop for (x1 y1 x2 y2) on (coerce coord-seq 'list) by #'cddddr do
	 (climi::with-transformed-position (tr x1 y1)
	   (climi::with-transformed-position (tr x2 y2)
	     (<+ `(gfg:draw-line ,gc
				 (gfs:make-point :x ,(floor x1)
						 :y ,(floor y1))
				 (gfs:make-point :x ,(floor x2)
						 :y ,(floor y2)))))))))

(defmethod medium-draw-polygon* ((medium graphic-forms-medium) coord-seq closed filled)
  (with-graphic-forms-medium (gc medium)
    (climi::with-transformed-positions (tr coord-seq)
      (let ((points-list (coordinates->points coord-seq)))
	(when (and closed (not filled))
	  (push (car (last points-list)) points-list))
	(if filled
	    (<+ `(gfg:draw-filled-polygon ,gc ',points-list))
	    (<+ `(gfg:draw-polygon ,gc ',points-list)))))))

(defmethod medium-draw-rectangle* ((medium graphic-forms-medium) left top right bottom filled)
  (with-graphic-forms-medium (gc medium)
    (climi::with-transformed-position (tr left top)
      (climi::with-transformed-position (tr right bottom)
	(let ((rect (coordinates->rectangle left top right bottom)))
	  (debug-prin1 "draw-rect" left top right bottom (medium-sheet medium))
	  (when (string= "15" *previous-draw-text*)
	    (when (= right 283)
					;(error "666")
	      (debug-prin1 666)
	      ))
	  (setf *previous-draw-text* "")
	  (if filled
	      (<+ `(gfg:draw-filled-rectangle ,gc ,rect))
	      (<+ `(gfg:draw-rectangle ,gc ,rect))))))))

(defmethod medium-draw-rectangles* ((medium graphic-forms-medium) position-seq filled)
  (with-graphic-forms-medium (gc medium)
    (loop for i below (length position-seq) by 4 do
	 (let ((x1 (floor (elt position-seq (+ i 0))))
	       (y1 (floor (elt position-seq (+ i 1))))
	       (x2 (floor (elt position-seq (+ i 2))))
	       (y2 (floor (elt position-seq (+ i 3)))))
	   (climi::with-transformed-position (tr x1 y1)
	     (climi::with-transformed-position (tr x2 y2)
	       (let ((rect (coordinates->rectangle x1 y1 x2 y2)))
		 (if filled
		     (<+ `(gfg:draw-filled-rectangle ,gc ,rect))
		     (<+ `(gfg:draw-rectangle ,gc ,rect))))))))))

(defun compute-quad-point (center-x height angle)
  (let* ((opp-len (/ height 2))
         (hyp-len (/ opp-len (sin angle)))
         (adj-len (sqrt (- (expt hyp-len 2) (expt opp-len 2)))))
    (<+ `(gfs:make-point :x ,(floor (+ center-x adj-len))
			 :y ,(floor opp-len)))))

(defun compute-arc-point (center-x center-y width height radians)
  (let ((angle (radians->degrees radians)))
    (multiple-value-bind (count remainder)
        (floor angle 360)
      (if (> count 0)
          (setf angle remainder)))
    (cond
      ((= angle 270)
       (<+ `(gfs:make-point :x ,(floor center-x)
			    :y ,(+ (floor center-y) (floor height 2)))))
      ((> angle 270)
       (compute-quad-point center-x height (- angle 270)))
      ((= angle 180)
       (<+ `(gfs:make-point :x ,(- (floor center-x) (floor width 2))
			    :y ,(floor center-y))))
      ((> angle 180)
       (compute-quad-point center-x height (- angle 180)))
      ((= angle 90)
       (<+ `(gfs:make-point :x ,(floor center-x)
			   :y ,(- (floor center-y) (floor height 2)))))
      ((> angle 90)
       (compute-quad-point center-x height(- angle 90)))
      ((= angle 0)
       (<+ `(gfs:make-point :x ,(+ (floor center-x) (floor width 2))
			    :y ,(floor center-y))))
      (t
       (compute-quad-point center-x height angle)))))

(defmethod medium-draw-ellipse* ((medium graphic-forms-medium)
                                 center-x center-y
                                 radius-1-dx radius-1-dy
                                 radius-2-dx radius-2-dy
                                 start-angle end-angle
                                 filled)
  (unless (or (= radius-2-dx radius-1-dy 0)
              (= radius-1-dx radius-2-dy 0))
    (error "MEDIUM-DRAW-ELLIPSE* not for non axis-aligned ellipses."))
  (with-graphic-forms-medium (gc medium)
    (climi::with-transformed-position
	(tr center-x center-y)
      (let* ((width (abs (+ radius-1-dx radius-2-dx)))
	     (height (abs (+ radius-1-dy radius-2-dy)))
	     (min-x (floor (- center-x width)))
	     (min-y (floor (- center-y height)))
	     (max-x (floor (+ center-x width)))
	     (max-y (floor (+ center-y height)))
	     (rect (coordinates->rectangle min-x min-y max-x max-y))
	     (start-pnt (compute-arc-point center-x center-y
					   width height
					   start-angle))
	     (end-pnt (compute-arc-point center-x center-y
					 width height
					 end-angle)))
	(if filled
	    (<+ `(gfg:draw-filled-pie-wedge ,gc ,rect ,start-pnt ,end-pnt)) 
	    (<+ `(gfg:draw-arc ,gc ,rect ,start-pnt ,end-pnt)))))))

(defmethod medium-draw-circle* ((medium graphic-forms-medium)
				center-x center-y radius start-angle end-angle
				filled)
  (medium-draw-ellipse* medium
			center-x center-y
			radius radius
			radius radius
			start-angle end-angle
			filled))

(defmethod text-style-ascent (text-style (medium graphic-forms-medium))
  (declare (ignore text-style))
  (let ((font (font-of medium)))
    (if font
      (with-server-graphics-context (gc (%target-of medium))
	(<+ `(gfg:ascent (gfg:metrics ,gc ,font))))
      1)))

(defmethod text-style-descent (text-style (medium graphic-forms-medium))
  (declare (ignore text-style))
  (let ((font (font-of medium)))
    (if font
      (with-server-graphics-context (gc (%target-of medium))
	(<+ `(gfg:descent (gfg:metrics ,gc ,font))))
      1)))

(defmethod text-style-height (text-style (medium graphic-forms-medium))
  (declare (ignore text-style))
  (let ((font (font-of medium)))
    (if font
      (with-server-graphics-context (gc (%target-of medium))
	(<+ `(gfg:height (gfg:metrics ,gc ,font))))
      1)))

(defmethod text-style-character-width (text-style (medium graphic-forms-medium) char)
  (declare (ignore text-style))
  (let ((font (font-of medium))
      ;  (width 1)
        (text (normalize-text-data char)))
    (if font
	(let ((width (with-server-graphics-context (gc (%target-of medium))
		       (<+ `(setf (gfg:font ,gc) ,font))
		       (<+ `(gfs:size-width (gfg:text-extent ,gc ,text))))))
	  width))
    1))

(defmethod text-style-width (text-style (medium graphic-forms-medium))
  (declare (ignore text-style))
  (let ((font (font-of medium)))
    (if font
      (with-server-graphics-context (gc (%target-of medium))
	(<+ `(gfg:average-char-width (gfg:metrics ,gc ,font))))
      1)))

(defmethod text-size ((medium graphic-forms-medium) string &key text-style (start 0) end)
  (if text-style
      (setf text-style
	    (merge-text-styles text-style (medium-default-text-style medium)))
      (setf text-style (medium-text-style medium)))
  (update-medium-font medium text-style)
  (setf string (normalize-text-data string))
  (with-server-graphics-context (gc (%target-of medium))
    (let ((font (font-of medium)))
      (<+ `(setf (gfg:font ,gc) ,font))
      (let ((metrics (<+ `(gfg:metrics ,gc ,font)))
	    (extent (<+ `(gfg:text-extent ,gc ,(subseq string
						  start
						  (or end (length string)))))))
	(values (<+ `(gfs:size-width ,extent))
		(<+ `(gfg:height ,metrics))
		(<+ `(gfs:size-width ,extent))
		(<+ `(gfg:height ,metrics))
		(<+ `(gfg:ascent ,metrics)))))))

(defmethod climi::text-bounding-rectangle*
    ((medium graphic-forms-medium) string &key text-style (start 0) end)
  ;; fixme, completely wrong
  (text-size medium string :text-style text-style :start start :end end))

(defparameter *previous-draw-text* "" )
(defmethod medium-draw-text* ((medium graphic-forms-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore align-x align-y toward-x toward-y transform-glyphs))
  (with-graphic-forms-medium (gc medium)
    (update-medium-font medium
			(merge-text-styles (medium-text-style medium)
					   (medium-default-text-style medium)))
    (setf string (normalize-text-data string))
    (let ((font (font-of medium)))
      (if font
	  (<+ `(setf (gfg:font ,gc) ,font)))
      (let ((ascent (<+ `(gfg:ascent (gfg:metrics ,gc ,font))))
	    (x (floor x))
	    (y (floor y)))
	(debug-prin1 "draw-text" string)
	(setf *previous-draw-text* string)
	(<+ `(gfg:draw-text ,gc
			    ,(subseq string start (or end (length string)))
			    (gfs:make-point :x ,x :y ,(- y ascent))
			    '(:transparent)))))))

(defmethod medium-buffering-output-p ((medium graphic-forms-medium))
  t)

(defmethod (setf medium-buffering-output-p) (buffer-p (medium graphic-forms-medium))
  buffer-p)

(defmethod medium-draw-glyph ((medium graphic-forms-medium) element x y
			      align-x align-y toward-x toward-y
			      transform-glyphs)
  ())

(defmethod medium-finish-output ((medium graphic-forms-medium))
  (when (image-of medium)
    (render-medium-buffer medium)))

(defmethod medium-force-output ((medium graphic-forms-medium))
  (when (image-of medium)
    (render-medium-buffer medium)))

(defmethod medium-clear-area ((medium graphic-forms-medium) left top right bottom)
  (with-graphic-forms-medium (gc medium :ink (medium-background medium))
    (let ((rect (coordinates->rectangle left top right bottom)))
      (<+ `(gfg:draw-filled-rectangle ,gc ,rect)))))

(defmethod medium-beep ((medium graphic-forms-medium))
  (<+ `(gfs::beep 750 300)))

(defmethod invoke-with-special-choices (continuation (medium graphic-forms-medium))
  (let ((sheet (medium-sheet medium)))
    (funcall continuation (sheet-medium sheet))))

(defmethod medium-miter-limit ((medium graphic-forms-medium))
  0)

;;; These and the following two are copy and modify from CLX
(defun %set-gc-clipping-region (medium gc)
  "Set graphic context GC (a Windows native object) the same as MEDIUM's setting."
  (declare (type graphic-forms-medium medium))
  (let ((clipping-region (climi::medium-device-region medium))
        (tmp (slot-value medium 'clipping-region-tmp)))
    (cond
      ((region-equal clipping-region +nowhere+)
       (gfg::set-clipping-region gc #()))
      ((typep clipping-region 'standard-rectangle)
       (multiple-value-bind (x1 y1 width height)
           (%region->clipping-values clipping-region)
         (setf (aref tmp 0) x1
               (aref tmp 1) y1
               (aref tmp 2) (+ x1 width)
               (aref tmp 3) (+ y1 height))
	 (gfg::set-clipping-region gc tmp)))
      (t
        (let ((rect-seq (%clipping-region->rect-seq clipping-region)))
          (when rect-seq
	    (gfg::set-clipping-region gc rect-seq)))))))

(defun %clipping-region->rect-seq (clipping-region)
  (typecase clipping-region 
    (area (multiple-value-list (%region->clipping-values clipping-region)))
    (t (loop 
          for region in (nreverse (mapcan
                                   (lambda (v) (unless (eq v +nowhere+) (list v)))
                                   (region-set-regions clipping-region
                                                       :normalize :y-banding)))
          collect (coerce (multiple-value-list (%region->clipping-values region)) 'vector)))))

(defun %region->clipping-values (region)
  (with-bounding-rectangle* (min-x min-y max-x max-y) region
    (values (round-coordinate min-x)
	    (round-coordinate min-y)
	    (round-coordinate max-x) 
	    (round-coordinate max-y))))


;;; medium-copy-area

(defmethod medium-copy-area ((from-drawable graphic-forms-medium) from-x from-y width height
                             (to-drawable graphic-forms-medium) to-x to-y)
  (let* ((from-sheet (medium-sheet from-drawable))
	 (from-transformation (sheet-native-transformation from-sheet))
	 (to-sheet (medium-sheet to-drawable))
	 (to-transformation (sheet-native-transformation to-sheet)))
    (climi::with-transformed-position (from-transformation from-x from-y)
      (climi::with-transformed-position (to-transformation to-x to-y)
	(multiple-value-bind (width height)
	    (transform-distance (medium-transformation from-drawable) width height)
	  (gfg:copy-image-area (image-of from-drawable) (image-of to-drawable)
			       :src-x (round-coordinate from-x) :src-y (round-coordinate from-y)
			       :dst-x (round-coordinate to-x) :dst-y (round-coordinate to-y)
			       :width (round width) :height(round height))
	  (add-medium-to-render to-drawable))))))

(defmethod medium-copy-area ((from-drawable graphic-forms-medium) from-x from-y width height
                             (to-drawable climi::pixmap) to-x to-y)
  (let* ((from-sheet (medium-sheet from-drawable))
  	 (from-transformation (sheet-native-transformation from-sheet)))
    (climi::with-transformed-position (from-transformation from-x from-y)
      (gfg:copy-image-area (image-of from-drawable) (climi::pixmap-mirror to-drawable)
			   :src-x (round-coordinate from-x) :src-y (round-coordinate from-y)
			   :dst-x (round-coordinate to-x) :dst-y (round-coordinate to-y)
			   :width (round width) :height(round height)))))

(defmethod medium-copy-area ((from-drawable climi::pixmap) from-x from-y width height
                             (to-drawable graphic-forms-medium) to-x to-y)
  (climi::with-transformed-position ((sheet-native-transformation (medium-sheet to-drawable))
  				     to-x to-y)

    (gfg:copy-image-area (climi::pixmap-mirror from-drawable) (image-of to-drawable)
    			 :src-x (round-coordinate from-x) :src-y (round-coordinate from-y)
    			 :dst-x (round-coordinate to-x) :dst-y (round-coordinate to-y)
    			 :width (round width) :height (round height)))
  (add-medium-to-render to-drawable)
  (render-pending-mediums))

(defmethod medium-copy-area ((from-drawable climi::pixmap) from-x from-y width height
                             (to-drawable climi::pixmap) to-x to-y)
  (gfg:copy-image-area (climi::pixmap-mirror from-drawable) (climi::pixmap-mirror to-drawable)
		       :src-x (round-coordinate from-x) :src-y (round-coordinate from-y)
		       :dst-x (round-coordinate to-x) :dst-y (round-coordinate to-y)
		       :width (round width) :height (round height)))
