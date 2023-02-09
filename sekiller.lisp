(defclass svg-file ()
  ((name :initarg :name :accessor name)
   (filename :initarg :filename :accessor filename); this may not be used
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (unit :initarg :unit :accessor unit)
   (title :initarg :title :accessor title)
   (desc :initarg :desc :accessor desc))
  (:documentation "This does not have initforms since it meant to be created with the constructor make-svg-file defined below."))

(defgeneric a4paper (svg-file &key landscape)
  (:documentation "
This sets the width and height parameters of a given svg file for a4paper. Optional keyword parameter is :landscape (default nil, any true value flips width and height).
The defaults are not defined here, because they cannot be defined here.
http://www.lispworks.com/documentation/HyperSpec/Body/03_db.htm
They need to be defined via defmethod."))

(defmethod a4paper ((s-f svg-file) &key (landscape nil))
  (if landscape
      (setf (height s-f) "210mm" (width s-f) "297mm")
      (setf (height s-f) "297mm" (width s-f) "210mm")))

  
(defgeneric boilerplate (svg-file)
  (:documentation "
Function to print the boilerplate for SVG files that goes to the top of the file. This is separate to increase flexibility."))

(defmethod boilerplate ((s-f svg-file))
  (format t "<?xml version=\"1.0\"?>~%")
  (format t  "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"~%")
  (format t  "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"))

(defun svg-boilerplate ()
  (format t "<?xml version=\"1.0\"?>~%")
  (format t  "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"~%")
  (format t  "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"))

(defun make-svg-file (name &key (width 0) (height 0) (unit "")
			     (filename "default-filename.svg")
			     (title "Default Title")
			     (desc "Default description"))
  (make-instance 'svg-file :name name
			   :filename filename
			   :width width :height height :unit unit
			   :title title :desc desc))

(defmacro with (tag &rest body)
  "This is from Graham's book (p. 260)"
  `(progn
     (format t "~&<~(~A~)>~%" ',tag)
     ,@body
     (format t "~&</~(~A~)>~%" ',tag)))

(defmacro with2 (tag &rest body)
  "This is from Graham's book (p. 260)"
  `(progn
     (format t "~&<~(~A~)" ',tag)
     ,@body
     (format t "/>~%")))

(defmacro as (tag content)
  "This is from Graham's book (p. 260)"
  `(format t "<~(~A~)>~A</~(~A~)>~%"
	   ',tag ,content ',tag))

(defmacro with-svg (s-f &rest body)
  `(progn
     (format t "~&<~(svg width=\"~A\" height=\"~A\"~)>~%"
	     (width ,s-f) (height ,s-f))
     (as desc (desc ,s-f))
     (as title (title ,s-f))
     ,@body
     (format t "~&</~(svg~)>~%")))


;;; a macro called defshape that allows defining a shape drawing function conveniently.
;;; this needs to be rewritten, a lot of the confusing pieces can actually
;;; reside outside this macro. Apperantly this is a common beginner's mistake
;;; using macros where a function would do 
(eval-when (:compile-toplevel)
  (defun add-supplied-p-to-arglist (styleargs)
    (loop for sa in styleargs
	  collect (append sa `(,(intern (format nil "~a-SUPPLIED-P" (first sa))))))))

;(defmacro defshape (elname ownargs styleargs (&optional (funname elname)))
;  (let ( (fullstyleargs (add-supplied-p-to-arglist styleargs)))
;    `(defun ,funname (&key ,@ownargs ,@fullstyleargs)
;       (progn
;	 (format t ,(format nil "<~(~a~) " elname))
;;;	 (format t "cx=\"~a\" " cx)
;	 ,@(loop for oa in ownargs
;		 collect `(format t
;				  ,(format nil "~(~a~)=\"~~,4f\" " (first oa))
;				  ,(first oa)))
;	 (if (or ,@(mapcar 'third fullstyleargs))
;	     (progn
;	       (format t "~& style=\"")
;;;	       (format t "~:[~; stroke: ~a;~]" stroke-supplied-p stroke)
;	       ,@(loop for sa in fullstyleargs
;		       collect `(format t
;					,(format nil "~~:[~~; ~(~a~): ~~a;~~]" (first sa))
;					,(third sa) ,(first sa)))
;	       (format t " \"")))
;	 (format t "/>~%")))))

;(defshape circle ( (cx 0.0) (cy 0.0) (r 1.0))
;    ( (stroke "black") (fill "none") (stroke-width 1.0))
;    (draw-circle))

(defun bunch-of-circles (cy)
  (let ((r 12))
    (loop for cx in (range 20 201 29)
	  do (with2 circle
		 (format t " r=\"~amm\" cx=\"~amm\" cy=\"~amm\"" r cx cy)
		 (format t " style=\"fill: none; stroke: black;")
		 (format t " stroke-dasharray: 5 3;\"")))))

(defun bunch-of-ellipses (cy &key (rotated nil))
  (let* ((r 12)
	 (golden-ratio 0.618)
	 (rx r) (ry r))
    (if rotated
	(setf rx (* rx golden-ratio))
	(setf ry (* ry golden-ratio)))
    (loop for cx in (range 20 201 29)
	  do (with2 ellipse
		 (format t " rx=\"~amm\" ry=\"~amm\"" rx ry)
		 (format t " cx=\"~amm\" cy=\"~amm\"" cx cy)
		 (format t " transform=\"rotate(~a, ~amm, ~amm)\"" (random 360) cx cy)
		 (format t " style=\"fill: none; stroke: black;")
		 (format t " stroke-dasharray: 5 3;\"")))))

(defun sekiller ()
  (let ((sek-svg (make-svg-file "sekiller" :unit "mm")))
    (a4paper sek-svg)
    (with-open-file (stream "nokta-birlestirmece-sekiller.svg"
			    :direction :output
			    :if-exists :supersede)
      (let ((*standard-output* stream))
	(svg-boilerplate)
	(with-svg sek-svg
	  (bunch-of-circles 30)
	  (bunch-of-circles 60)
	  (bunch-of-ellipses 90)
	  (bunch-of-ellipses 120 :rotated t)
	  )))))
    

(defun range (a &optional (b 0 b-supplied-p) (c 0 c-supplied-p))
  "An over simple implementation of range. It assumes increasing integers etc."
  (if c-supplied-p
      (loop for i from a below b by c
	    collect i)
      (if b-supplied-p
	  (loop for i from a below b
		collect i)
	  (loop for i from 0 below a
		collect i))))
