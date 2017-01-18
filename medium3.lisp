(in-package :clim-graphic-forms)


;;; graphic-forms-medium class and finalizer

(defclass graphic-forms-medium (basic-medium)
  ((image
    :accessor medium-image
    :initform nil)
   (gcontext
    :accessor medium-gcontext
    :initform nil)
   (font
    :accessor medium-font
    :initform nil)
   ;; Copy from CLX
   (clipping-region-tmp
    :initform (vector 0 0 0 0)
    :documentation "This object is reused to avoid consing in the
 most common case when configuring the clipping region.")))

(defmethod initialize-instance :after ((medium graphic-forms-medium) &rest initargs)
  (ensure-medium-gcontext medium)
  (update-medium-gcontext medium)
  (update-medium-font medium))


;;; Set graphic context according to medium background, foreground, line-style.

(defun ensure-medium-gcontext (medium)
  (unless (medium-gcontext medium)
    (let* ((region (sheet-region (medium-sheet medium)))
	   (width (floor (bounding-rectangle-max-x region)))
	   (height (floor (bounding-rectangle-max-y region))))
     (setf (medium-image medium)
	   (<+ `(make-instance 'gfg:image
			       :size (gfs:make-size :width ,width :height ,height)))))
    (setf (medium-gcontext medium) (<+ `(make-graphics-context ,(medium-image medium))))))

(defun update-medium-gcontext (medium)
  (update-medium-gcontext-foreground medium)
  (update-medium-gcontext-background medium)
  (update-medium-gcontext-line-style medium)
  (update-medium-gcontext-clipping-region medium))

(defun medium-absolute-ink (medium)
  (cond
    ((subtypep (class-of (medium-ink medium)) (find-class 'climi::opacity))
     (medium-foreground medium)) ; see discussion of opacity in design.lisp
    ((eql (medium-ink medium) +foreground-ink+)
     (medium-foreground medium))
    ((eql (medium-ink medium) +background-ink+)
     (medium-background medium))
    ((eql (medium-ink medium) +flipping-ink+)
     (error "+flipping-ink+ is currently not implemented in MCCLIM-GRAPHIC-FORMS"))
    (t (medium-ink medium))))

(defun absolute-ink-to-color (ink)
  (multiple-value-bind (red green blue) (clim:color-rgb ink)
    (gfg:make-color :red (min (truncate (* red 256)) 255)
		    :green (min (truncate (* green 256)) 255)
		    :blue (min (truncate (* blue 256)) 255))))

(defun medium-ink-to-color (medium ink)
  (absolute-ink-to-color (mediu-absolute-ink medium)))

(defun update-medium-gcontext-foreground (medium)
  "Update MEDIUM's GCONTEXT foreground color according to MEDIUM-INK."
  (let ((foreground-color (absolute-ink-to-color (medium-absolute-ink medium))))
    (<+ `(setf (gfg:foreground-color ,(medium-gcontext medium)) ,foreground-color))))

(defun update-medium-gcontext-background (medium)
  (let ((background-color (absolute-ink-to-color (medium-background medium))))
    (<+ `(setf (gfg:background-color ,(medium-gcontext medium)) ,background-color))))

(defun update-medium-gcontext-line-style (medium)
  ())


;;; Clipping region helper functions
;;; This section is copy and modify from CLX

(defun update-medium-gcontext-clipping-region (medium)
  "Set the graphic context's clipping region of MEDIUM (graphic context is a Windows native object) to the same as MEDIUM's setting.
Should be called before any drawing methods on GRAPHIC-FORMS-MEDIUM."
  (declare (type graphic-forms-medium medium))
  (let ((clipping-region (climi::medium-device-region medium))
        (tmp (slot-value medium 'clipping-region-tmp)))
    (cond
      ((region-equal clipping-region +nowhere+)
       (<+ `(gfg::set-clipping-region ,(medium-gcontext medium) #())))
      ((typep clipping-region 'standard-rectangle)
       (multiple-value-bind (x1 y1 width height)
           (region->clipping-values clipping-region)
         (setf (aref tmp 0) x1
               (aref tmp 1) y1
               (aref tmp 2) (+ x1 width)
               (aref tmp 3) (+ y1 height))
	 (<+ `(gfg::set-clipping-region ,(medium-gcontext medium) ,tmp))))
      (t
        (let ((rect-seq (clipping-region->rect-seq clipping-region)))
          (when rect-seq
	    (<+ `(gfg::set-clipping-region ,(medium-gcontext medium) ,rect-seq))))))))

(defun clipping-region->rect-seq (clipping-region)
  (typecase clipping-region 
    (area (multiple-value-list (region->clipping-values clipping-region)))
    (t (loop 
          for region in (nreverse (mapcan
                                   (lambda (v) (unless (eq v +nowhere+) (list v)))
                                   (region-set-regions clipping-region
                                                       :normalize :y-banding)))
          collect (coerce (multiple-value-list (region->clipping-values region)) 'vector)))))

(defun region->clipping-values (region)
  (with-bounding-rectangle* (min-x min-y max-x max-y) region
    (values (round-coordinate min-x)
	    (round-coordinate min-y)
	    (round-coordinate max-x) 
	    (round-coordinate max-y))))


;;; CLIM text-style and Windows native font object mapping
(defun text-style-to-font (medium text-style)
  (let* ((gc (medium-gcontext medium))
	 (old-font-data
	  (when (medium-font medium)
	    (<+ `(gfg:data-object ,(medium-font medium) ,gc))))
	 (new-font (%text-style-to-font gc text-style old-font-data)))
    new-font))

(defun update-medium-font (medium)
  "Synchronize MEDIUM's font (a native object) to MEDIUM's TEXT-STYLE. Should be called before 
text related methods on GRAPHIC-FORMS-MEDIUM class."
  (let* ((new-font (text-style-to-font medium (medium-text-style medium)))) 
    (when new-font
      (setf (medium-font medium) new-font))))

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

(defun %text-style-to-font (gc text-style old-data)
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


;;; set graphic-forms-medium properties

(defmethod (setf medium-font) :before (font (medium graphic-forms-medium))
  (let ((old-font (medium-font medium)))
    (when (and old-font (not (<+ `(gfs:disposed-p ,old-font))))
      (<+ `(gfs:dispose ,old-font)))))

(defmethod (setf medium-font) :after (font (medium graphic-forms-medium))
  (when font
    (<+ `(setf (gfg:font ,(medium-gcontext medium)) ,font))))

(defmethod (setf medium-text-style) :after (text-style (medium graphic-forms-medium))
  (update-medium-font medium))

(defmethod (setf medium-line-style) :after (line-style (medium graphic-forms-medium))
  (update-medium-gcontext-line-style medium))

(defmethod (setf medium-ink) :after (ink (medium graphic-forms-medium))
  (update-medium-gcontext-foreground medium))

(defmethod (setf medium-foreground) :after (foreground (medium graphic-forms-medium))
  (update-medium-gcontext-foreground medium))

(defmethod (setf medium-background) :after (background (medium graphic-forms-medium))
  (update-medium-gcontext-background medium))

(defmethod (setf climi::medium-device-region) :after (device-region (medium graphic-forms-medium))
  (update-medium-gcontext-clipping-region medium))


;;; medium draw methods

(defmacro with-graphic-forms-medium ((medium) &body body)
  `(let ((tr (sheet-native-transformation (medium-sheet ,medium)))
	 (gc (medium-gcontext medium)))
     ,@body))

(defmacro with-filled-gcontext ((medium) &body body)
  "Draw in a context that filled color is the same as foreground color. On Windows, default is filled color is background color."
  (alexandria:with-gensyms (gc color old-color m)
    `(let* ((,m ,medium)
	    (,gc (medium-gcontext ,m))
	    (,color (absolute-ink-to-color (medium-absolute-ink ,m)))
	    (,old-color (<+ `(gfg:background-color ,,gc))))
       (unwind-protect
	    (progn (<+ `(setf (gfg:background-color ,,gc) ,,color))
		   ,@body)
	 (<+ `(setf (gfg:background-color ,,gc) ,,old-color))))))

(defmethod medium-draw-point* ((medium graphic-forms-medium) x y)
  (with-graphic-forms-medium (medium)
    (climi::with-transformed-position (tr x y)
      (<+ `(gfg:draw-point ,gc (gfs:make-point :x ,(floor x) :y ,(floor y)))))))

(defmethod medium-draw-points* ((medium graphic-forms-medium) coord-seq)
  (loop for (x y) on (coerce coord-seq 'list) by #'cddr do
       (medium-draw-point* medium x y)))

(defmethod medium-draw-line* ((medium graphic-forms-medium) x1 y1 x2 y2)
  (with-graphic-forms-medium (medium)
    (climi::with-transformed-position (tr x1 y1)
      (climi::with-transformed-position (tr x2 y2)
	(<+ `(gfg:draw-line ,gc
			    (gfs:make-point :x ,(floor x1)
					    :y ,(floor y1))
			    (gfs:make-point :x ,(floor x2)
					    :y ,(floor y2))))))))

(defmethod medium-draw-lines* ((medium graphic-forms-medium) coord-seq)
  (loop for (x1 y1 x2 y2) on (coerce coord-seq 'list) by #'cddddr do
       (medium-draw-line* medium x1 y1 x2 y2)))

(defmethod medium-draw-polygon* ((medium graphic-forms-medium) coord-seq closed filled)
  (with-graphic-forms-medium (medium)
    (climi::with-transformed-positions (tr coord-seq)
      (let ((points-list (coordinates->points coord-seq)))
	(when (and closed (not filled))
	  (push (car (last points-list)) points-list))
	(if filled
	    (with-filled-gcontext (medium)
	      (<+ `(gfg:draw-filled-polygon ,gc ',points-list)))
	    (<+ `(gfg:draw-polygon ,gc ',points-list)))))))

(defmethod medium-draw-rectangle* ((medium graphic-forms-medium) left top right bottom filled)
  (with-graphic-forms-medium (medium)
    (climi::with-transformed-position (tr left top)
      (climi::with-transformed-position (tr right bottom)
	(let ((rect (coordinates->rectangle left top right bottom)))
	  (if filled
	      (with-filled-gcontext (medium)
		(<+ `(gfg:draw-filled-rectangle ,gc ,rect)))
	      (<+ `(gfg:draw-rectangle ,gc ,rect))))))))

(defmethod medium-draw-rectangles* ((medium graphic-forms-medium) position-seq filled)
  (loop for (x1 y1 x2 y2) on (coerce coord-seq 'list) by #'cddddr do
       (medium-draw-rectangle* x1 y1 x2 y2 filled)))

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
    (error "MEDIUM-DRAW-ELLIPSE* not implemented for non axis-aligned ellipses in MCCLIM-GRAPHIC-FORMS."))
  (with-graphic-forms-medium (medium)
    (climi::with-transformed-position (tr center-x center-y)
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
	    (with-filled-gcontext (medium)
	      (<+ `(gfg:draw-filled-pie-wedge ,gc ,rect ,start-pnt ,end-pnt))) 
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


;;; text-style related methods 

(defun text-style-metric (text-style medium metric-fn)
  (let ((font (text-style-to-font medium text-style)))
    (if font
      (<+ `(,metric-fn (gfg:metrics ,(medium-gcontext medium) ,font)))
      (if (medium-font medium)
	  (<+ `(,metric-fn (gfg:metrics ,(medium-gcontext medium) ,(medium-font medium))))
	  1))))

(defmethod text-style-ascent (text-style (medium graphic-forms-medium))
  (text-style-metric text-style medium 'gfg:ascent))

(defmethod text-style-descent (text-style (medium graphic-forms-medium))
  (text-style-metric text-style medium 'gfg:descent))

(defmethod text-style-height (text-style (medium graphic-forms-medium))
  (text-style-metric text-style medium 'gfg:height))

(defmethod text-style-width (text-style (medium graphic-forms-medium))
  (text-style-metric text-style medium 'gfg:average-char-width))

(defmacro with-temporary-font ((medium temp-font) &body body)
  (alexandria:with-gensyms (m gc medium-font font)
    `(let* ((,m ,medium)
	    (,gc (medium-gcontext ,m))
	    (,medium-font (medium-font ,m))
	    (,font ,temp-font))
       (unwind-protect
	    (progn
	      (<+ `(setf (gfg:font ,,gc) ,,font))
	      ,@body)
	 (<+ `(setf (gfg:font ,,gc) ,,medium-font))))))

(defmethod text-style-character-width (text-style (medium graphic-forms-medium) char)
  (let ((gc (medium-gcontext medium))
	(font (text-style-to-font medium text-style))
        (text (normalize-text-data char)))
    (if font
	(with-temporary-font (medium font)
	  (<+ `(gfs:size-width (gfg:text-extent ,gc ,text))))
	(if (medium-font medium)
	    (<+ `(gfs:size-width (gfg:text-extent ,gc ,text)))
	    1))))

;;; FIXME: not consider #\Newline
(defmethod text-size ((medium graphic-forms-medium) string &key text-style (start 0) end)
  (let ((gc (medium-gcontext medium)))
    (flet ((text-metrics (gc font string start end)
	     (let ((metrics (<+ `(gfg:metrics ,gc ,font)))
		   (extent (<+ `(gfg:text-extent ,gc ,(subseq string
							      start
							      (or end (length string)))))))
	       (values (<+ `(gfs:size-width ,extent))
		       (<+ `(gfg:height ,metrics))
		       (<+ `(gfs:size-width ,extent))
		       (<+ `(gfg:height ,metrics))
		       (<+ `(gfg:ascent ,metrics))))))
     (if text-style
	 (let ((font (text-style-to-font medium text-style)))
	   (with-temporary-font (medium font)
	     (text-metrics gc font string start end)))
	 (text-metrics gc (medium-font medium) string start end)))))

;;; TODO: what is left, right in font-text-extents mean?
(defmethod climi::text-bounding-rectangle*
    ((medium graphic-forms-medium) string &key text-style (start 0) end)
  (error "TEXT-BOUNDING-RECTANGLE not implemented in MCCLIM-GRAPHIC-FORMS")
  ;(text-size medium string :text-style text-style :start start :end end)
  )

(defmethod medium-draw-text* ((medium graphic-forms-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)

  (declare (ignore align-x align-y toward-x toward-y transform-glyphs))
  (setf string (normalize-text-data string))
  (with-graphic-forms-medium (medium)
    (climi::with-transformed-position (tr x y)
    (let ((font (medium-font medium))) 
      (let ((ascent (<+ `(gfg:ascent (gfg:metrics ,gc ,font))))
	    (x (floor x))
	    (y (floor y)))
	(<+ `(gfg:draw-text ,gc
			    ,(subseq string start (or end (length string)))
			    (gfs:make-point :x ,x :y ,(- y ascent))
			    ;'(:transparent)
			    )))))))


;;; Medium buffering

(defmethod medium-buffering-output-p ((medium graphic-forms-medium))
  t)

(defmethod (setf medium-buffering-output-p) (buffer-p (medium graphic-forms-medium))
  buffer-p)

(defmethod medium-draw-glyph ((medium graphic-forms-medium) element x y
			      align-x align-y toward-x toward-y
			      transform-glyphs)
  ())

(defparameter *medium-origin* (<+ `(gfs:make-point)))

(defun draw-medium-image-to-canvas (medium)
  (let ((mirror (climi::port-lookup-mirror (port (medium-sheet medium)) (medium-sheet medium))))
    (when (typep mirror 'gfw-top-level)
      (<+ `(gfg:draw-image ,(canvas-gcontext mirror) ,(medium-image medium) ,*medium-origin*)))))

(defmethod medium-finish-output ((medium graphic-forms-medium))
  (draw-medium-image-to-canvas medium))

;; actually called this
(defmethod medium-force-output ((medium graphic-forms-medium))
  (draw-medium-image-to-canvas medium))


;;; Misc

(defmethod medium-clear-area ((medium graphic-forms-medium) left top right bottom)
  ;;; FIXME: what if line style?
  (with-graphic-forms-medium (medium)
    (with-transformed-position (tr left top)
      (with-transformed-position (tr right bottom)
	(let ((rect (coordinates->rectangle left top right bottom)))
	  ;; not in with-filled-gcontext because we want filled with background
	  (<+ `(gfg:draw-filled-rectangle ,gc ,rect)))))))

(defmethod medium-beep ((medium graphic-forms-medium))
  (<+ `(gfs::beep 750 300)))

(defmethod invoke-with-special-choices (continuation (medium graphic-forms-medium))
  (let ((sheet (medium-sheet medium)))
    (funcall continuation (sheet-medium sheet))))

(defmethod medium-miter-limit ((medium graphic-forms-medium))
  0)

