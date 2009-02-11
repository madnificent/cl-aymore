;;;; This is the micro HTML-generation-library.  It is possible to make this more efficient by expanding the functions, but that can be implemented later.
;;;; TODO: Throw all the existing tags from the xhtml standard in here and implement them
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
	       (loop while (keywordp (first content))
		  do (progn (setf keys (concatenate 'list keys (list (first content) (second content))))
			    (setf content (rest (rest content)))))
	       (values keys content))))
    (multiple-value-bind (keys content)
	(key-vals list)
      (multiple-value-bind (start-tag end-tag)
	  (tagify (first list) keys content)
	(if end-tag
	    (format nil "~A~{~A~}~A" 
		    (string-downcase (string start-tag))
		    (map 'list (lambda (x)
				 (if (stringp x)
				     x
				     (htmlify x)))
			 content)
		    (string-downcase (string end-tag)))
	    start-tag)))))

;; The following makes it easy to define new tags, yet it does not yet make it efficient.
;; It does, however, give us a place in which we may macro-expand or compiler-macro-expand to precomputed everything we know already
(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (tag '(html head title body h1 h2 h3 h4 h5 h6 table tr td p div ul ol li span strong a br form input textarea style b i em hidden))
    (eval `(declaim (inline ,tag)))
    (eval `(defun ,tag (&rest tag-data)
	     ;;(format nil "TAG :: ~A." (nstring-downcase (string (quote ,tag))))
	     (htmlify (concatenate 'list (cons (quote ,tag) tag-data)))))))

(defun link-to-page (name page &optional (page-options nil) &rest path-options)
  "Links to the given page"
  (a :href (apply 'build-path
		  (apply 'minions.routing:handler-url page page-options)
		  path-options)
     name))

(defun redirect-to-page (page &key (page-options nil) (path-options nil))
  "Redirects to the given page"
  (hunchentoot:redirect (apply 'build-path 
			       (apply 'minions.routing:handler-url page page-options) 
			       path-options)))

(defun button-to (page name url-options key-values)
  "Creates a button to the given page, with the given caption on the button and the given url-options and key-values"
  (form :method "post" :action (apply 'minions.routing:handler-url page url-options)
	(loop for i from 0 below (length key-values)
	   by 2
	   collect
	     (hidden :name (elt key-values i) :value (elt key-values (1+ i))))
	(input :type "submit" :value name)))

(defun bttn-to (page button-name &key variable-name (url-options nil))
  "Creates a simple form-button to the given page"
  (form :method "post" :action (apply 'minions.routing:handler-url page url-options)
	(if variable-name
	    (input :type "submit" :name variable-name :value button-name)
	    (input :type "submit" :value button-name))))

(defun build-path (path &rest options)
  "Creates a path for a page with the given key-values options"
  (format nil "~A~@[?~]~1@*~{~A=~A~^&~}" path options))

;;  (format nil "~A~@[?~]~:*~{~A=~A~^&~}" path options))  ;; this would be a better option
;; the if-construct would be understandable, yet I seem to like the complicated format call for now.
;;   (if options
;;       (format nil "~A?~{~A=~A~^&~}" path options)
;;       path))