(defclass svg-file ()
  ((width :initform 0 :initarg :width :accessor width)
   (height :initform 0 :initarg :height :accessor width)
   (filename :initform "default-svg-file.svg" :initarg :filename)
   (title :initform "Default Title" :initarg :title)
   (desc :initform "Default Description" :initarg :desc)))

(defgeneric a4paper (svg-file &key ((resolution 300) (landscape nil)))
  (:documentation "
This sets the width and height parameters of a given svg file for a4paper. Optional keyword parameters are :resolution (default 300, given in dpi) and :landscape (default nil, any true value flips width and height)."))

(defmethod a4paper ((s-f svg-file) &key ((resolution 300) (landscape nil)))
  )
