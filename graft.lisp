(in-package :clim-graphic-forms)

(defclass graphic-forms-graft (standard-graft)
  ())

(defmethod graft-width ((graft graphic-forms-graft) &key (units :device))
  (let ((window (graphic-forms-port-screen (port graft))))
    (let ((size (<+ `(gfw:size ,window))))
      (with-server-graphics-context (gc window)
	(ecase units
	  (:device       (<+ `(gfs:size-width ,size)))
	  (:millimeters  (<+ `(gfs::get-device-caps (gfs:handle ,gc) gfs::+horzsize+)))
	  (:inches       (floor (<+ `(gfs:size-width ,size))
				(<+ `(gfs::get-device-caps (gfs:handle ,gc) gfs::+logpixelsx+))))
	  (:screen-sized 1))))))

(defmethod graft-height ((graft graphic-forms-graft) &key (units :device))
  (let ((window (graphic-forms-port-screen (port graft))))
    (let ((size (<+ `(gfw:size ,window))))
      (with-server-graphics-context (gc window)
	(ecase units
	  (:device       (<+ `(gfs:size-height ,size)))
	  (:millimeters  (<+ `(gfs::get-device-caps (gfs:handle ,gc) gfs::+vertsize+)))
	  (:inches       (floor (<+ `(gfs:size-height ,size))
				(<+ `(gfs::get-device-caps (gfs:handle ,gc) gfs::+logpixelsy+))))
	  (:screen-sized 1))))))
