(defsystem :mcclim-graphic-forms
  :depends-on (:clim :graphic-forms :lparallel
	       :mcclim-full-mirrored-standard)
  :serial t 
  :components
  ((:file "package")
   (:file "graphic-forms-server")
   (:file "utils")
   (:file "graft")
   (:file "port3")
   (:file "medium")
   (:file "gadgets2")
;   (:file "pixmap")
   (:file "frame-manager")))
