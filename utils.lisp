;;;; utils.lisp
;;;;
;;;; Copyright (c) 2016 Ian Marshall

(in-package :x-plane-udp)

(defparameter *xchar-size* 1)
(defparameter *xint-size* 4)
(defparameter *xfloat-size* 4)
(defparameter *xdouble-size* 8)

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
     ,@body))

(defmacro until>= (form num &key (attempts 0) (start 0))
  `(do ((result ,start ,form)
	(trap 0 (incf trap)))
       ((or (>= result ,num) (> trap ,attempts)) 'done)
     (print result)))

(defmacro until-true (form &optional (attempts 0))
  `(do ((result nil ,form)
	(trap 0 (incf trap)))
       ((or (not (null result)) (> trap ,attempts)) result)))

(defun listpairs (lst)
  (let ((i 0)
	(mapped ()))
    (mapc (lambda (l r)
	    (incf i)
	    (if (oddp i)
		(push (list l r) mapped)))
	  lst
	  (cdr lst))
    mapped))

(defun mappairs (func lst)
    (let ((i 0)
	(mapped ()))
    (mapc (lambda (l r)
	    (incf i)
	    (if (oddp i)
		(push (funcall func l r)  mapped)))
	  lst
	  (cdr lst))
    mapped))

(defun mapp (func lst)
    (let ((i 0))
      (mapc (lambda (l r)
	      (incf i)
	      (if (oddp i)
		(funcall func l r)))
	  lst
	  (cdr lst))))

(defun string-to-bytes (str &key (nut t) (type '(unsigned-byte 8)))
  (let* ((len (if nut (+ 1 (length str))(length str)))
	 (strarr (make-array len :element-type type :initial-element 0)))
    (dotimes (i (length str))
      (setf (elt strarr i) (char-code (char str i))))
    strarr))

(defun bytes-to-string (bytes &key (nut t))
  (let ((trunk (if nut (subseq bytes 0 (- (length bytes) 1)) bytes)))
    (map 'string #'code-char trunk)))

(defun upcase-sep-from-right (sep str)
  (let ((last-sep (position sep str :from-end t))) 
    (string-upcase (subseq str (+ 1 (position sep str
					 :from-end t
					 :end last-sep))))))

(defmacro with-byte-buffer (name
			    (&key (type '(unsigned-byte 8))
				  (size 413))
			    &rest body)
  `(let* ((,name (make-array ,size
			     :element-type ',type
			     :adjustable t
			     :initial-element 0)))
     ,@body))

;; Return the addition of the passed group of types and possibly amounts
(defmacro x-bs (&rest types)
  (let ((total 0))
    (mapc (lambda (pack)
	    (if (listp pack)
		(setq total (+ total (xbyte-size (car pack) (car (cdr pack)))))
		(setq total (+ total (xbyte-size pack)))))
	  types)
    `,total))

(defun xbyte-size (type &optional (count 1))
  (* count (cond  
	     ((equalp type 'float) *xfloat-size*)
	     ((equalp type 'int) *xint-size*)
	     ((equalp type 'double) *xdouble-size*)
	     ((equalp type 'char) *xchar-size*)
	     (t 0))))

(defmacro to-byte-arr (byte-me
			 array
			 &key (be t)(size 32)(byte-size 8))
  (let ((loader ())
	(pos 0))
    (dotimes (b (/ size byte-size))
      (if be 
	  (setf pos (* b byte-size))
	  (setf pos (- size (* (+ b 1) byte-size))))
      (push `(ldb (byte ,byte-size ,pos) ,byte-me) loader)
      (push `(aref ,array ,b) loader))
    (push 'setf loader)
    ;(print loader)
    `(progn
       ,loader
       ,array)))

(defmacro from-byte-arr (array &key (be t)(byte-size 8))
  (let* ((size (* (length array) byte-size))
	 (u (gensym))
	 (loaded (let ((loader ())
		       (pos 0))
		   (dotimes (i (length array))
		     (if be
			 (setf pos (* i byte-size))
			 (setf pos (- size (* (+ i 1) byte-size))))	 
		     ;(push `(aref ,array ,i) loader)
		     (push (aref array i) loader)
		     (push `(ldb (byte ,byte-size ,pos) ,u) loader))
		   (push 'setf loader)
		   loader)))
    ;;(print loaded)
    `(let ((,u 0))
       ,loaded
       ,u)))

(defun read-long (byte-array &optional (offset 0))
  (let ((long 0))
    (setf (ldb (byte 8 0) long) (aref byte-array offset)
          (ldb (byte 8 8) long) (aref byte-array (+ offset 1))
          (ldb (byte 8 16) long) (aref byte-array (+ offset 2))
          (ldb (byte 8 24) long) (aref byte-array (+ offset 3)))
    long))

(defun read-long-be (byte-array &optional (offset 0))
  (let ((long 0))
    (setf (ldb (byte 8 24) long) (aref byte-array offset)
          (ldb (byte 8 16) long) (aref byte-array (+ offset 1))
          (ldb (byte 8 8) long) (aref byte-array (+ offset 2))
          (ldb (byte 8 0) long) (aref byte-array (+ offset 3)))
    long))

(declaim (inline u32-to-sf))
(defun u32-to-sf% (x)
  (declare (optimize (speed 3) (compilation-speed 0) (debug 0))
           (type (unsigned-byte 32) x))
  (if (>= x #x80000000)
      (sb-kernel:make-single-float (- x #x100000000))
      (sb-kernel:make-single-float x)))

(declaim (inline u32-to-sf))
(defun u32-to-sf (x)
  (declare (optimize (speed 3) (compilation-speed 0) (debug 0))
           (type (unsigned-byte 32) x))
  (sb-kernel:make-single-float x))

(defun zipit (a b &optional (type nil))
  (let ((zipped ()))
    (if (and a b)
	(mapc (lambda (x y)
		(if (equal type 'list)
		    (setf zipped (append zipped (list (list x y))))
		    (setf zipped (append zipped (list x y)))))
	      a b))
    zipped))

(defun init-syms (syms inits)
  (if (numberp inits)
      (mapcar (lambda (sym)
		`(,sym ,inits))
	      syms)	     
      (mapcar (lambda (sym init)
		`(,sym ,init))
	      syms
	      inits)))


