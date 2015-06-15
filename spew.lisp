(defpackage :spew 
  (:use :cl))

(in-package :spew)


;;; OO interface

(defparameter *default-spewer* nil
  "Singleton created on the fly when API invoked without a spewer
instance, and reset to nil after output.")

(defun default-spewer ()
  "Defaulting accessor to the default spewer instance."
  (or *default-spewer*
      (setf *default-spewer* (make-instance 'spewer))))

(defun (setf default-spewer) (value)
  (setf *default-spewer* value))

(defclass spewer ()
  ((col-id :initarg :col-id :initform 0 :accessor col-id)
   (row-id :initarg :row-id :initform 0 :accessor row-id)
   (custom-id :initarg :custom-id :initform 0 :accessor custom-id)
   (running-css :initarg :running-css :initform nil :accessor running-css))
  (:documentation "Objects of this type encapsulate a web page composition process."))

(defmacro first-or-default-spewer (list)
  "Treat a list as starting with an optional parameter of type spewer,
which is obtained or defaulted."
  `(if (typep (first ,list) 'spewer)
       (pop ,list)
       (default-spewer)))


;;; Simple things

(defun simple-div-css (self css) 
  (format nil "#custom-div-~a { ~a }" 
	  (custom-identifier self)
	  css))

(defun simple-div-html (self x) 
  (let ((css (getf x :styles)))
    (if css 
	(progn
          (push (simple-div-css self css) (running-css self))
	  (format nil "<div id='custom-div-~a'>~a</div>" 
		  (custom-id self)
		  (getf x :content)))
	(format nil "<div>~a</div>" 
		(getf x :content)))))


;;; Output functions

(defun write-files (html-content &key spewer css-file html-file)
  (with-open-file (css-stream css-file
                              :direction :output 
                              :if-does-not-exist :create
                              :if-exists :overwrite)
    (with-open-file (html-stream html-file 
                                 :direction :output
                                 :if-does-not-exist :create
                                 :if-exists :overwrite) 
      (write-output html-content
                    :spewer spewer
                    :css-stream css-stream
                    :html-stream html-stream))))

(defun write-strings (html-content &key spewer)
  (let ((html-string (make-array '(0) :element-type 'base-char
                                      :fill-pointer 0 :adjustable t))
        (css-string (make-array '(0) :element-type 'base-char
                                     :fill-pointer 0 :adjustable t)))
    (with-output-to-string (css-stream css-string)
      (with-output-to-string (html-stream html-string)
        (write-output html-content
                      :spewer spewer
                      :css-stream css-stream
                      :html-stream html-stream)))
    (values html-string css-string)))

(defun write-output (html-content &key spewer css-stream html-stream)
  (let ((self (or spewer (default-spewer)))
        (defaulted (null spewer)))
    (format css-stream "~{~a~%~}" (reverse (running-css self)))
    (format html-stream
            "<html><head><link href='test.css' rel='stylesheet' type='text/css'></head><body>~a</body></html>"
            (getf html-content :content))
    (when defaulted
      (setf (default-spewer) nil))))


;;; Composition functions

(defun cols (cols-list)
  (let* ((self (first-or-default-spewer cols-list))
         (html (cols-html self cols-list))
         (css (cols-css self cols-list)))
    (push css (running-css self))
    (list :content html)))

(defun rows (rows-list)
  (let* ((self (first-or-default-spewer rows-list))
         (html (rows-html self rows-list))
         (css (rows-css self rows-list)))
    (push css (running-css self))
    (list :content html)))

(defun cols-identifier (&optional self) 
  (incf (col-id (or self (default-spewer)))))

(defun rows-identifier (&optional self) 
  (incf (row-id (or self (default-spewer)))))

(defun custom-identifier (&optional self)
  (incf (custom-id (or self (default-spewer)))))

(defun cols-html (self cols-list)
  (format nil "<div id='col-container-~a'>~%~{~a~%~}</div><div style='clear:both;'></div>" 
	  (cols-identifier self)
	  (mapcar (lambda (elt)
                    (simple-div-html self elt))
                  cols-list)))

(defun rows-html (self rows-list) 
  (format nil "<div id='row-container-~a'>~%~{~a~%~}</div>" 
	  (rows-identifier self)
	  (mapcar (lambda (elt)
                    (simple-div-html self elt))
                  rows-list)))

(defun cols-css (self cols-list) 
  (format nil "#~a>div {~%float:left;box-sizing:border-box;~%border:1px solid #000;~%width: ~a%;~%}" 
	  (format nil "col-container-~a" (col-id self))
	  (floor 100 (length cols-list))))

(defun rows-css (self rows-list) 
  (format nil "#~a>div {~%width:100%;box-sizing:border-box;~%border:1px solid #000;~%height: ~a%;~%}" 
	  (format nil "row-container-~a" (row-id self))
	  (floor 100 (length rows-list))))


;;; Examples

(defun example-strings () 
  (write-strings
   (cols 
    (list 
     (rows 
      '((:content "Test div please ignore")
	(:content "Second test div")))
     (rows 
      '((:content "Test div again"
	 :styles "background-color: #f00;")
	(:content "Test div again")
	(:content "Test div three")))))))


(defun example-files () 
  (write-files 
   (cols 
    (list 
     (rows 
      '((:content "Test div please ignore")
	(:content "Second test div")))
     (rows 
      '((:content "Test div again"
	 :styles "background-color: #f00;")
	(:content "Test div again")
	(:content "Test div three")))))
   :css-file "test.css"
   :html-file "test.html"))

