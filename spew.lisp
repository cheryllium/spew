(defpackage :spew 
  (:use :cl))
(in-package :spew)

(defparameter *col-id* 0)
(defparameter *row-id* 0)
(defparameter *custom-id* 0)

(defparameter *running-css* nil)

(defun simple-div-css (css) 
  (format nil "#custom-div-~a { ~a }" 
	  (custom-identifier)
	  css))

(defun simple-div-html (x) 
  (let ((css (getf x :styles)))
    (if css 
	(progn
          (push (simple-div-css css) *running-css*)
	  (format nil "<div id='custom-div-~a'>~a</div>" 
		  *custom-id*
		  (getf x :content)))
	(format nil "<div>~a</div>" 
		(getf x :content)))))

(defun make-files (html-content) 
    (with-open-file (stream "test.css"
			    :direction :output 
			    :if-does-not-exist :create
			    :if-exists :overwrite)
      (format stream "~{~a~%~}" *running-css*))
    (with-open-file (stream "test.html" 
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :overwrite) 
      (format stream 
	      "<html><head><link href='test.css' rel='stylesheet' type='text/css'></head><body>~a</body></html>"
	      (getf html-content :content))))

(defun cols (cols-list) 
  (let ((html (cols-html cols-list))
	(css (cols-css cols-list)))
    (push css *running-css*)
    (list :content html)))

(defun rows (rows-list)
  (let ((html (rows-html rows-list))
	(css (rows-css rows-list)))
    (push css *running-css*)
    (list :content html)))

(defun cols-identifier () 
  (incf *col-id*))

(defun rows-identifier () 
  (incf *row-id*))

(defun custom-identifier () 
  (incf *custom-id*))

(defun cols-html (cols-list) 
  (format nil "<div id='col-container-~a'>~%~{~a~%~}</div><div style='clear:both;'></div>" 
	  (cols-identifier)
	  (mapcar 
	   #'simple-div-html
	   cols-list)))

(defun rows-html (rows-list) 
  (format nil "<div id='row-container-~a'>~%~{~a~%~}</div>" 
	  (rows-identifier)
	  (mapcar 
	   #'simple-div-html 
	   rows-list)))

(defun cols-css (cols-list) 
  (format nil "#~a>div {~%float:left;box-sizing:border-box;~%border:1px solid #000;~%width: ~a%;~%}" 
	  (format nil "col-container-~a" *col-id*)
	  (floor 100 (length cols-list))))
(defun rows-css (rows-list) 
  (format nil "#~a>div {~%width:100%;box-sizing:border-box;~%border:1px solid #000;~%height: ~a%;~%}" 
	  (format nil "row-container-~a" *row-id*)
	  (floor 100 (length rows-list))))







(defun example () 
  (make-files 
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
