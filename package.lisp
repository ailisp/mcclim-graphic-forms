(in-package :common-lisp-user)

(defpackage :clim-graphic-forms
  (:nicknames :clim-gf)
  (:use :clim :clim-lisp :clim-backend)
  (:import-from :clim-standard

		:standard-full-mirrored-sheet-mixin 
		:sheet-mirror-region
		:standard-graft
		:standard-event-port-mixin
		:pointer-grab-sheet))
