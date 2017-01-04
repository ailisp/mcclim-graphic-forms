(in-package :clim-gf)

(define-application-frame buttons () ()
  (:menu-bar nil)
  (:layouts
   (default
       (vertically ()
	   (labelling (:label "McCLIM Demos"
			   :text-style (make-text-style :sans-serif :roman :huge)
			   :align-x :center))
;	 (make-pane 'push-button :label "6")
	   ))))


