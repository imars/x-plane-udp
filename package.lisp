;;;; package.lisp
;;;;
;;;; Copyright (c) 2016 Ian Marshall

(defpackage #:x-plane-udp
  (:use #:cl :usocket :ieee-floats)
  (:export #:with-xplane-udp
	   #:start
	   #:stop
	   #:updater
	   #:update
	   #:send
	   #:field-names
	   #:field-inits
	   #:decode-rrefs
	   #:encode-drefs
	   #:with-rrefs
	   #:with-drefs
	   #:with-data
	   #:with-xplane-refs))

