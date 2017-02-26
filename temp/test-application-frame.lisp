(in-package :clim-gf)

(define-application-frame superapp ()
  ()
  (:panes
   (int :interactor :height 400 :width 600))
  (:layouts
   (default int)))

(defun app-main ()
  (run-frame-top-level (make-application-frame 'superapp)))
