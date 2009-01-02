;;;;  This is the micro html-generation-library.  It is possible to make this more efficiently by using functions, but that can be implemented later.
(defpackage minions.html
  (:use :common-lisp)
  (:export :htmlify))

(in-package :minions.html)

(declaim (inline tagify))
(defun tagify (keyword options &optional (contentp nil))
  "Creates the printed tag for the given keyword"
  (if contentp
      (values (format nil "<~A~{ ~A=\"~A\"~}>" keyword options)
	      (format nil "</~A>" keyword))
      (format nil "<~A ~{~A=~A ~}/>" keyword options)))

(defun htmlify (list)
  "Transforms a sexp to an html document."
  (labels ((key-vals (list) 
	     (let ((content (rest list))
		   (keys nil))
	       (loop while (keywordp (first content))
		  do (progn (setf keys (concatenate 'list keys (list (first content) (second content))))
			    (setf content (rest (rest content)))))
	       (values keys content))))
    (multiple-value-bind (keys content)
	(key-vals list)
      (multiple-value-bind (start-tag end-tag)
	  (tagify (first list) keys content)
	(if end-tag
	    (format nil "~A~%~{~A~}~%~A" 
		    start-tag
		    (map 'list (lambda (x)
				 (if (stringp x)
				     x
				     (htmlify x)))
			 content)
		    end-tag)
	    start-tag)))))
      
