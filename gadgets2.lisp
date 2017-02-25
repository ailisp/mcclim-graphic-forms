(in-package :clim-graphic-forms)

(defmethod destroy-mirror ((port graphic-forms-port) (gadget value-gadget))
  (let ((mirror (climi::port-lookup-mirror port gadget)))
    (climi::port-unregister-mirror port gadget mirror)))

(defmethod destroy-mirror ((port graphic-forms-port) (gadget action-gadget))
  (let ((mirror (climi::port-lookup-mirror port gadget)))
    (climi::port-unregister-mirror port gadget mirror)))

;;;
;;; layout
;;;

(defmethod compose-space ((gadget action-gadget) &key width height)
  (declare (ignore width height))
  (let ((mirror (climi::port-lookup-mirror (port gadget) gadget))
        (pref-size (<+ `(gfs:make-size :width 100 :height 100))))
    (if mirror
      (setf pref-size (<+ `(gfw:preferred-size ,mirror -1 -1)))
      (progn
        (setf mirror (<+ `(make-instance 'gfw:button :parent ,(sheet-mirror (sheet-parent gadget)) :text ,(gadget-label gadget))))
        (setf pref-size (<+ `(gfw:preferred-size ,mirror -1 -1)))
        (<+ `(gfs:dispose ,mirror))
        (setf mirror nil)))
    (make-space-requirement :width (<+ `(gfs:size-width ,pref-size))
                            :height (<+ `(gfs:size-height ,pref-size)))))

;;;
;;; gadgets
;;;

(defmethod handle-repaint :before ((pane graphic-forms-pane-mixin) region)
  (declare (ignore region))
  
  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
    ;; This actually only need for panes that not full of contents (leave something as background)
    ;; maybe better did it in Graphic-Forms
    ;; must avoid using clim:draw-rectangle here to avoid recording
    (with-graphic-forms-medium (gc (sheet-medium pane) :ink +background-ink+)
      (let ((rect (coordinates->rectangle x1 y1 x2 y2)))
  	(<+ `(gfg:draw-filled-rectangle ,gc ,rect))))))
