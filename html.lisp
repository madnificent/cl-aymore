;;;; This is the micro HTML-generation-library.  It is possible to make this more efficient by expanding the functions, but that can be implemented later.
;;;; TODO: Throw all the existing tags from the xhtml standard in here and implement them
(in-package :CLaymore.html)

(declaim (inline tagify))
(defun tagify (keyword options &optional (contentp nil))
  "Creates the printed tag for the given keyword"
  (let ((lowered-key (string-downcase (string keyword)))
	(lowered-options (loop for (k v) on options by #'cddr collect
			      `(,(string-downcase (string k)) ,v))))
    (if contentp
	(values (format nil "<~A~{ ~{~A=\"~A\"~}~}>" lowered-key lowered-options)
		(format nil "</~A>" lowered-key))
	(format nil "<~A~{ ~{~A=\"~A\"~}~} />" lowered-key lowered-options))))

(defun htmlify (list)
  "Transforms a sexp to an html document."
  (when (stringp list)
    (return-from htmlify list))
  (when (eql list nil)
    (return-from htmlify ""))
  (when (stringp (first list))
    (return-from htmlify
      (concatenate 'string (first list) (htmlify (rest list)))))
  (when (listp (first list))
    (return-from htmlify
      (concatenate 'string
		   (apply 'concatenate 'string (map 'list 'htmlify (first list)))
		   (htmlify (rest list)))))
  (labels ((key-vals (list) 
	     (let ((content (rest list))
		   (keys nil))
	       (loop while (keywordp (first content)) do 
		    (progn
		      (setf keys 
			    (concatenate 'list
					 keys
					 (list (first content) (second content))))
		      (setf content (rest (rest content)))))
	       (values keys content))))
    (multiple-value-bind (keys content)
	(key-vals list)
      (multiple-value-bind (start-tag end-tag)
	  (tagify (first list) keys content)
	(if end-tag
	    (format nil "~A~{~A~}~A" 
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (tag '(html head title body h1 h2 h3 h4 h5 h6 table tr th td p div ul ol li span strong a br hr form input textarea style b i em hidden label))
    (eval `(declaim (inline ,tag)))
    (eval `(defun ,tag (&rest tag-data)
	     ;;(format nil "TAG :: ~A." (nstring-downcase (string (quote ,tag))))
	     (htmlify (concatenate 'list (cons (quote ,tag) tag-data)))))))
