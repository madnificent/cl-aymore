(in-package :CLaymore.html.full)

(defun build-path (path &rest options)
  "Creates a path for a page with the given key-values options"
  (format nil "~A~@[?~]~1@*~{~A=~A~^&~}" path options))
;;  (format nil "~A~@[?~]~:*~{~A=~A~^&~}" path options))  ;; this would be a better option
;; the if-construct would be understandable, yet I seem to like the complicated format call for now.
;;   (if options
;;       (format nil "~A?~{~A=~A~^&~}" path options)
;;       path))


(defun link-to-page (name page &optional (page-options nil) &rest path-options)
  "Links to the given page"
  (let ((hurl (apply 'build-path 
		     (apply 'CLaymore.routing:handler-url page page-options)
		     path-options)))
    (a :href hurl
       name)))

(defun redirect-to-page (page &key (page-options nil) (path-options nil))
  "Redirects to the given page"
  (hunchentoot:redirect (apply 'build-path 
			       (apply 'CLaymore.routing:handler-url page page-options) 
			       path-options)))

(defun button-to (page name url-options key-values)
  "Creates a button to the given page, with the given caption on the button and the given url-options and key-values"
  (form :method "post" :action (apply 'CLaymore.routing:handler-url page url-options)
	(loop for i from 0 below (length key-values)
	   by 2
	   collect
	     (hidden :name (elt key-values i) :value (elt key-values (1+ i))))
	(input :type "submit" :value name)))

(defun bttn-to (page button-name &key variable-name (url-options nil))
  "Creates a simple form-button to the given page"
  (form :method "post" :action (apply 'CLaymore.routing:handler-url page url-options)
	(if variable-name
	    (input :type "submit" :name variable-name :value button-name)
	    (input :type "submit" :value button-name))))

;;;;;;;;;;;;;;;
;; form helpers

(defun text-field (var print-varname? &rest options)
  (if print-varname?
      (list (strong var)
	    (apply 'input `(:type "text" :name ,var ,@options)))
      (apply 'input `(:type "text" :name ,var ,@options))))

(defun text-area (var print-varname? content &rest options)
  (if print-varname?
      (list (strong var)
	    (apply 'textarea `(:name ,var ,@options ,@(or content '("")))))
      (apply 'textarea `(:name ,var ,@options ,@(or content '(""))))))

(defun submit-button (&rest options)
  (apply 'input `(:type "submit" ,@options)))
