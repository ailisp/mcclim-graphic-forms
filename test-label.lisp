(in-package :climi)

(define-application-frame buttons () ()
  (:menu-bar nil)
  (:layouts
   (default
       (vertically ()
		   (labelling (:label "MMMM"
				      :text-style
				      (make-text-style :sans-serif :roman :huge)
				      :align-x :center)
		     (make-pane 'push-button :label "MMMM" :text-style
				      (make-text-style :sans-serif :roman :huge)
))
	 ;; (make-pane 'push-button :label "MMMM" :text-style
;; 				      (make-text-style :sans-serif :roman :huge)
;; )gad
	   ))))


