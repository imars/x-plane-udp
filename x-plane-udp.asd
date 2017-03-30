;;;; x-plane-udp.asd
;;;;
;;;; Copyright (c) 2016 Ian Marshall

(asdf:defsystem #:x-plane-udp
  :description "UDP Interface to the X-Plane flight simulator."
  :author "Ian Marshall <i.marshall@mac.com>"
  :license "Specify license here"
  :depends-on (#:usocket
               #:ieee-floats)
  :serial t
  :components ((:file "package")
	       (:file "utils")
               (:file "x-plane-udp")))

