(in-package :common-lisp-user)

(defpackage :clim-graphic-forms
  (:nicknames :clim-gf)
  (:use :clim :clim-lisp :clim-backend)
  (:import-from :clim-standard

		:standard-single-mirrored-sheet-mixin
		:standard-full-mirrored-sheet-mixin 
		:sheet-mirror-region
		:standard-graft
		:standard-port
		:standard-event-port-mixin
		:standard-handled-event-port-mixin 
		:pointer-grab-sheet))
