(defpackage :minions.routing
  (:use :common-lisp
	:cl-ppcre)
  (:export :get-handler
	   :add-route
	   :add-tree))

(in-package :minions.routing)

(defvar *table* nil)

;; h1. How the routes are defined internally
;; The routes are internally represented as a tree of closures.  The closures must be able to handle pages in two directions.  The first direction is the one from the address to the calling function, the second one returns the url for a certain page.
;;
;; h2. From url to page
;; For this system, the routing table calls the closure, with :page-handler.  When a page-handler is found, it is returned, otherwise nil is returned.  The page-handler is a lambda-function (or keyword) that will be called to display the correct page.
;;
;; h2. From page to url
;; This is the more complicated system.  The routing table calls the closure with :page-url.  An unspecified number of options may be given to this function, which can be used to fill in the required detailing arguments (the closure is not forced to care about these).  The system will ask each page to return a valid URL, when a url can be constructed, the system returns the URL for the given page.
;;
;; h1. How the routes can be manipulated
;; Every route closure should be able to handle the extra input given to it.  This basically means that you can tell the lambda to connect a certain url to the given target.  This basically means that you'll give the closure a base-path and a lambda that will handle that base-path.  If the base-path is allready handled by a certain lambda, then that lambda will be replaced by the given lambda.
;;
;; h2. How can users add routes?
;; The routing system will handle a list of strings differently than other routes.  Basically, the list of strings is split and the node will be added to the lowest possible handler.  When no other lambda's can handle the page, a lambda will be created for that level, as to allow other nodes to pick up on them later.

(defconstant +URLSPLIT+ #\/)

;; The node does NOT contain the leading slash!
(defun node (base handler)
  (let ((subnodes nil))
    (lambda (request &rest args)
      (cond ( ;; We are printing a table
	     (eql request :tableize)
	     `(,base ,handler
		     ,@(loop for node in subnodes collect (funcall node :tableize))))
	    ( ;; We are searching for a page-handler
	     (eql request :page-handler)
	     (let* ((split (cl-ppcre:split (string +URLSPLIT+) (first args) :limit 2))
		    (first-url-section (first split))
		    (rest-url-section (second split)))
	       (when (string= base first-url-section)
		 (if (not rest-url-section)
		     handler
		     (loop named find-handler
			for subnode in subnodes do
			(let ((found-handler (funcall subnode :page-handler rest-url-section)))
			  (when found-handler 
			    (return-from find-handler found-handler))))))))
	    ( ;; We are searching for a matching path
	     (eql request :page-url)
	     (if (eql handler (first args))
		 (progn (format T "Match returning: ~A~%" base)
			base)
		 (loop named find-url
		    for subnode in subnodes do
		      (let ((url (funcall subnode :page-url (first args))))
			(when url
			  (format T "returning ~A~%"(concatenate 'string base (string +URLSPLIT+) url))
			  (return-from find-url (concatenate 'string base (string +URLSPLIT+) url)))))))
	    ( ;; Add subnode
	     (eql request :add-handler)
	     (let* ((split (cl-ppcre:split (string +URLSPLIT+) (first args) :limit 2))
		    (first-url-section (first split))
		    (rest-url-section (second split)))
	       (format T "first-url-section: ~A~%rest-url-section: ~A~%" first-url-section rest-url-section)
	       (when (string= base first-url-section)
		 (if (not rest-url-section)
		     (setf handler (second args))
		     (block dispatched-assignment
		       (format T "dispatched-assignment~%")
		       (loop for subnode in subnodes do
			    (when (funcall subnode :add-handler rest-url-section (second args))
			      (return-from dispatched-assignment T)))
		       (format T "no suitable node fount~%")
		       (push (new-node rest-url-section (second args)) subnodes))))))))))
;; 		       (let* ((split (cl-ppcre:split (string +URLSPLIT+) rest-url-section :limit 2))
;; 			      (new-base (first split))
;; 			      (new-rest (second split)))
;; 			 (format T "adding node to newly built handler~%")
;; 			 (format T "end node? ~A~%" new-rest)
;; 			 (if new-rest
;; 			     (progn (push (node new-base nil) subnodes)
;; 				    (funcall (first subnodes) :add-handler rest-url-section (second args)))
;; 			     (push (node new-base (second args)) subnodes))))))))))))

(defun new-node (path handler)
  "Creates a new node for the given path and handler"
  (let* ((split (cl-ppcre:split (string +URLSPLIT+) path :limit 2))
	 (base (first split))
	 (rest (second split)))
    (format T "adding node to newly built handler~%")
    (format T "end node? ~A~%" rest)
    (if rest
	(let ((node (node base nil)))
	  (funcall node :add-handler path handler)
	  node)
	(node base handler))))

	       
(defun page-handler (path)
  (loop for table in *table* do
       (let ((r (funcall table :page-handler path)))
	 (when r (return-from page-handler r)))))
(defun page-url (page)
  (loop for table in *table* do
       (let ((r (funcall table :page-url page)))
	 (when r (return-from page-url r)))))
(defun add-page (path handler)
  (loop for table in *table* do
       (when (funcall table :add-handler path handler)
	 (return-from add-page *table*)))
  (push (new-node path handler) *table*))

(defun clear-table ()
  (setf *table* nil))
(defun swap-table (table)
  (setf *table* table))
(defun print-table ()
  (loop for table in *table* collect
       (funcall table :tableize)))