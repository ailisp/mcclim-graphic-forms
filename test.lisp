(in-package :clim-gf)

;;;
;;; (run-frame-top-level (make-application-frame 'buttons))
;;;

(define-application-frame buttons () ()
  (:menu-bar nil)
  (:layouts
    (default
      (vertically (:equalize-width t)
        (make-pane 'push-button :label "First")))))
