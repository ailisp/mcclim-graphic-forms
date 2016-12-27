(in-package :clim-graphic-forms)

(defclass graphic-forms-frame-manager (frame-manager)
  ())

(defmethod make-pane-1 ((fmgr graphic-forms-frame-manager) (frame application-frame) type &rest initargs)
  (apply #'make-pane-2 type :manager fmgr :frame frame :port (port frame) initargs))

(defmethod adopt-frame :after ((fmgr graphic-forms-frame-manager) (frame application-frame))
  ())

(defmethod note-space-requirements-changed :after ((graft graphic-forms-graft) pane)
  #+nil (<+ `(gfs::debug-format "space requirements changed: ~a~%" ,pane)))

