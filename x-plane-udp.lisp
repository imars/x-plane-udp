;;;; x-plane-udp.lisp
;;;;
;;;; Copyright (c) 2016 Ian Marshall

;; All types are double-float and coereced thusly from x-plane
;; due to floating point overflow problems.

(in-package #:x-plane-udp)

(defparameter *pl-len+null* 5)
(defparameter *pl-len* (- *pl-len+null* 1)) 
(defparameter *rref-prologue* #(82 82 69 70))
(defparameter *dref-prologue* #(68 82 69 70))
(defparameter *data-prologue* #(68 65 84 65))
(defparameter *rref-index-offset* *pl-len+null*) 
(defparameter *field-size-bytes* 4)
(defparameter *xstrDim* 500)

(defmacro with-xplane-udp ((&key
			    (local-host "127.0.0.1")
			    (local-port 49007)
			    (host "127.0.0.1")
			    (port 49000)
			    (control-port 49008)
			    (timeout 3)
			    (select nil)
			    (select-attempts 5)
			    (encode nil)
			    (recieve nil)))
  (let* ((server (gensym "server"))
	 (sender (gensym "sender"))
	 (thread-output (gensym "thread-out"))
	 (start (gensym "start"))
	 (stop (gensym "stop"))
	 (connect (gensym "connect"))
	 (close (gensym "close"))
	 (transmit (gensym "transmit"))
	 (request (gensym "request"))
	 (handler recieve))
    `(let* ((,server nil)
	    (,sender nil)
	    (,thread-output nil)
	    (,connect (lambda (&optional (send-port ,control-port))
			(if ,sender (socket-close ,sender))
			(setq ,sender
			      (socket-connect nil nil
					      :local-host ,local-host
					      :local-port send-port
					      :protocol :datagram
					      :element-type '(unsigned-byte 8)))))
	    (,transmit (lambda ()
			 (let ((bufs (funcall ,encode)))
			   (dotimes (i (length bufs))
			     (let ((buf (aref bufs i)))
			       (if buf
				   (socket-send ,sender buf (length buf)
						:host ,host :port ,port)))))))
	    (,close (lambda ()
		      (if ,sender
			  (progn
			    (socket-close ,sender)))))
	    (,request (lambda (&optional (send-port ,local-port))
			(unwind-protect
			     (let ((rrefs (if ,select (funcall ,select) nil))
				   (sock
				    (socket-connect nil nil
						    :local-host ,local-host
						    :local-port send-port
						    :protocol :datagram
						    :element-type '(unsigned-byte 8))))
			       (if rrefs 
				   (dolist (req rrefs)
				     (until-true
				      (equal (socket-send sock req (length req)
							  :host ,host :port ,port)
					     (length req))
				      ,select-attempts)))
			  (socket-close sock)))))
	    (,start (lambda (&optional (thread-out ,*standard-output*))
		      (setq ,thread-output thread-out)
		      (if ,request (funcall ,request))
		      (funcall ,connect)
		      (setq ,server (socket-server ,local-host ,local-port ,handler nil
						   :in-new-thread t
						   :protocol :datagram
						   :timeout ,timeout))
		      (format t "UDP Server ~a started." ,server)))
    	    (,stop (lambda ()
		     (funcall ,close)
		     (if ,server
			 (progn
			   (format t "Stopping X-Plane UDP server ~a~%." ,server)
			   (sb-thread:terminate-thread ,server))))))
       (values ,start ,stop ,transmit ,connect ,close ,request))))

(defun field-names (refs &optional (ext ""))
  (mapcar (lambda (ref)
	    (let ((ref-str (if (stringp ref) ref (car ref))))
	      (with-input-from-string
		  (s (concatenate 'string
				  (upcase-sep-from-right #\/ ref-str)
				  ext))
		(read s))))
	    refs))

(defun field-inits (fields)
  (mapcar (lambda (field)
		    `(,field 0.0))
	  fields))

(defun decode-rrefs (fields buffer)
  (let ((decoder ())
	(index 0))
    (mapc (lambda (field)
	    (setf decoder
		  (append decoder
			  `(,field (coerce
				    (decode-float32
				     (read-long ,buffer
						(+ *pl-len+null* *xint-size*
						   (* ,index (+ *xint-size*
								*xfloat-size*)))))
				    'double-float))))
	    (incf index))
	  fields)
    (push 'setq decoder)))

(defmacro with-rrefs (rrefs (&optional (output *standard-output*)) &rest body)
  (let* ((request (gensym "request"))
	 (recieve (gensym "floats"))
	 (fields (field-names rrefs))
	 (lets (field-inits fields))
	 (bufparm (gensym "bufparm"))
	 (decoder (decode-rrefs fields bufparm)))
    `(let ,lets
       (let* ((,recieve (lambda (,bufparm)
			  (declare (type (simple-array (unsigned-byte 8) *) ,bufparm))
			  (let ((*standard-output* ,output))
			    (cond
			      ((not (null (search *rref-prologue*
						  ,bufparm :end2 *pl-len*)))
			       ,decoder))
			    ,@body)))
	      (,request (lambda ()
			  (let ((index 0)
				(intbuf (make-array 4 :element-type '(unsigned-byte 8)))
				(size (+ *pl-len+null*
					 (* 2 *xint-size*)
					 (* 400 *xchar-size*))))
			    (mapcar (lambda (packet)
				      (let ((packed
					     (make-array size
							 :element-type
							 '(unsigned-byte 8))))
					(setf (subseq packed 0) (string-to-bytes "RREF"))
					(setf (subseq packed *pl-len+null*)
					      (to-byte-arr (second packet) intbuf))
					(setf (subseq packed
						      (+ *pl-len+null* *xint-size*))
					      (to-byte-arr index intbuf))
					(setf (subseq packed
						      (+ *pl-len+null*
							 (* 2 *xint-size*)))
					      (string-to-bytes (first packet)))
					(incf index)
					packed))
				    ',rrefs)))))
	 (values ,recieve ,request)))))

;; Encode a value in a float field into its DREF buffer
(defun encode-dref (field buffers index &optional (offset *pl-len+null*))
  `(setf (aref (aref ,buffers ,index) ,(+ 3 offset))
	 (ldb (byte 8 24) (encode-float32 ,field))
	 (aref (aref ,buffers ,index) ,(+ 2 offset))
	 (ldb (byte 8 16) (encode-float32 ,field))
	 (aref (aref ,buffers ,index) ,(+ 1 offset))
	 (ldb (byte 8 8) (encode-float32 ,field))
	 (aref (aref ,buffers ,index) ,offset)
	 (ldb (byte 8 0) (encode-float32 ,field))))

;; Only send bytes when underlying field is changed
(defun encode-if-changed (old new buffers send-buffers)
  (let ((index -1))
    (if (and old new)
	(mapcar (lambda (o n)
		  (incf index)
		  `(if (and (not (eql ,o ,n)) (not (null ,n)))
		       (progn
			 (setq ,o ,n)
			 ,(encode-dref o buffers index)
			 (setf (aref ,send-buffers ,index)
			       (aref ,buffers ,index)))
		       (setf (aref ,send-buffers ,index) nil)))
		old new))))

;; X-Plane DREF tranmission
(defmacro with-drefs (drefs (&optional (output *standard-output*)) &rest body)
  (let* ((buffers (gensym "buffers"))
	 (send-buffers (gensym "send-buffers"))
	 (encode (gensym "encode"))
	 (update (gensym "update"))
	 (drefstrs (mapcar (lambda (ref)
			     (if (listp ref) (car ref) ref))
			   drefs))
	 (fields (field-names drefs))
	 (params (field-names drefs "-p"))
	 (prev (field-names drefs "-prev"))
	 (lets (append (field-inits fields)
		       (field-inits prev))))
    `(let ,lets
       (let* ((,send-buffers (make-array (length ',fields)))
	      (,buffers (make-array (length ',fields)))
	      (,encode (lambda ()
			 (let ((*standard-output* ,output))
			   ,@body
			   ,@(encode-if-changed prev fields buffers send-buffers)
			   ,send-buffers)))
	      (,update (lambda (&optional ,@(init-syms params 0.0d0))
			 (setq ,@(zipit fields params)))))

	 ;; Create and initialize transmission buffers with xp ref string and prologue
	 (dotimes (i (length ',drefs))
	   (setf (elt ,buffers i)
		 (make-array (+ *xfloat-size* *xstrDim* *pl-len+null*)
			     :element-type '(unsigned-byte 8)
			     :initial-element 0))
	   (replace (elt ,buffers i) *dref-prologue* :end1 *pl-len*)
	   (replace (elt ,buffers i)
		    (string-to-bytes (elt ',drefstrs i))
		    :start1 (+ *pl-len+null* *xfloat-size*)
		    :end1 (+ (+ *pl-len+null* *xfloat-size*)
			     (length (elt ',drefstrs i)))))
	 (values ,encode ,update)))))

#|
(with-xplane-refs 
	    (:drefs ("sim/flightmodel/engine/ENGN_thro[0]"
		     "sim/flightmodel/engine/ENGN_thro[1]"
		     "sim/flightmodel/engine/ENGN_thro[2]"
		     "sim/flightmodel/engine/ENGN_thro[3]")
	     :onsend ((print engine/engn_thro[0]))
	     :onrecieve ((print (list "Airspeed:" position/true_airspeed))))
  (setq starter start)
  (setq stopper stop)
  (setq updater update))
|#

(defmacro with-xplane-refs ((&key
			     (rrefs '(("sim/flightmodel/position/true_airspeed" float 1)
				      ("sim/flightmodel/position/magpsi" float 1)))
			     (onrecieve nil)
			     (drefs '(("sim/flightmodel/engine/ENGN_thro[0]")))
			     (onsend nil)
			     (local-host "127.0.0.1")
			     (local-port 49007)
			     (host "127.0.0.1")
			     (port 49000)
			     (control 49008)
			     (timeout 3)) &rest body)
  (let ((updater (gensym))
	(encode (gensym))
	(rec-mutex (gensym))
	(snd-mutex (gensym)))
    `(let ((,rec-mutex (sb-thread:make-mutex :name "Recieve lock"))
	   (,snd-mutex (sb-thread:make-mutex :name "Send lock")))
       (multiple-value-bind (recieve select)
	   (with-rrefs ,rrefs (,*standard-output*)
		       (sb-thread:with-recursive-lock (,rec-mutex)
			 ,@onrecieve)
		       (values))
	 (multiple-value-bind (,encode ,updater)
	     (with-drefs ,drefs (,*standard-output*)
			 (sb-thread:with-recursive-lock (,snd-mutex)
			   ,@onsend))
	   (multiple-value-bind (start stop send connect close request)
	       (with-xplane-udp (:select select
					 :recieve recieve
					 :encode ,encode
					 :local-host ,local-host
					 :local-port ,local-port
					 :host ,host
					 :port ,port
					 :control-port ,control
					 :timeout ,timeout))
	     (declare (ignore connect close request))
	     (let ((update (lambda (&optional ,@(init-syms (field-names drefs) 0.0))
			     (funcall ,updater ,@(field-names drefs))
			     (funcall send)
			     (values))))
	       ,@body
	       (values start stop update send))))))))




#|

(multiple-value-bind (engage disengage)
		 (with-xplane-rrefs (("sim/flightmodel/position/vpath" float 1)
				     ("sim/flightmodel/position/hpath" float 1)
				     ("sim/flightmodel/position/phi" float 1)
				     ("sim/flightmodel/misc/h_ind" float 1)
				     ("sim/flightmodel/position/vh_ind" float 1)
				     ("sim/flightmodel/position/y_agl" float 1)
				     ("sim/flightmodel/position/P" float 1)
				     ("sim/flightmodel/position/Q" float 1)
				     ("sim/flightmodel/position/R" float 1)
				     ("sim/flightmodel/position/P_dot" float 1)
				     ("sim/flightmodel/position/Q_dot" float 1)
				     ("sim/flightmodel/position/R_dot" float 1)
				     ) () 
				     (print (list "pitch (vpath)" position/vpath  
						  "yaw (hpath)" position/hpath  
						  "roll (phi)" position/phi
						  "AGL (y_agl)" position/y_agl
						  "alt (h_ind)" misc/h_ind
						  "VVI (vh_ind)" position/vh_ind
						  "P" position/P
						  "Q" position/Q
						  "R" position/R
						  "P_dot" position/P_dot
						  "Q_dot" position/Q_dot
						  "R_dot" position/R_dot
						  ))
				     (values))
	       (setq start engage)
	       (setq stop disengage))

|#































































