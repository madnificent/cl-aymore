;;;; This is the micro HTML-generation-library.  It is possible to make this more efficient by expanding the functions, but that can be implemented later.
;;;; TODO: Throw all the existing tags from the xhtml standard in here and implement them
(defpackage minions.html
  (:use :common-lisp)
  (:export :htmlify
	   :html 
	   :head 
	   :title 
	   :body 
	   :h1 
	   :h2 
	   :h3 
	   :h4 
	   :h5 
	   :h6 
	   :p 
	   :div 
	   :span 
	   :strong))

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

;; The following makes it easy to define new tags, yet it does not yet make it efficient.
;; It does, however, give us a place in which we may macro-expand or compiler-macro-expand to precomputed everything we know already
(eval-when (:compile-toplevel :load-toplevel)
  (dolist (tag '(html head title body h1 h2 h3 h4 h5 h6 p div span strong))
    (eval `(defun ,tag (&rest tag-data)
	     (format nil "TAG :: ~A." (nstring-downcase (string (quote ,tag))))
	     (htmlify (concatenate 'list (cons (quote ,tag) tag-data)))))))