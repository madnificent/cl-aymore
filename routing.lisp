(defpackage :minions.routing
  (:use :common-lisp
	:cl-ppcre)
  ;; handlers on node-objects
  (:export :set-page-handler
	   :page-handler
	   :page-url
	   :tableize)
  ;; handlers on the general system
  (:export :clear-table
	   :get-page-handler
	   :get-page-url
	   :add-page
	   :print-table))

(in-package :minions.routing)

(defvar *table* nil)

;; TODO :: This documentation is outdated, it needs a rewrite for the new CLOS-system
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
;;
;; h2. How to extend the routing system
;; A node will add a routing handler to the given path, when :add-table path subtable is called.
;;
;; h1. An alternative rewrite
;; It might be nicer to create a rewrite of this system in CLOS.  If there is ever going to be a speed issue, then it will probably be handled by creating a separate implementation for the recognising of URLs in both directions.  That would -in effect- make the slower CLOS implementation quite acceptable.

(defconstant +URLSPLIT+ #\/)

(defclass node () ()
  (:documentation "This contains a node with some general actions that are described to it.  Subclasses of this, can be attached to other nodes."))

(defclass fixed-path-node (node)
  ((base :initarg :base
	 :accessor base)
   (handler :initarg :handler
	    :accessor handler
	    :initform nil)
   (subnodes :initarg :subnodes
	     :accessor subnodes
	     :initform nil))
  (:documentation "This is the simplest useable node.  It allows you to define a string that it will 'eat' from the path, and a handler to return if it could find a match."))

(defclass grouping-node (node)
  ((subnodes :initarg :subnodes
	     :accessor subnodes
	     :initform nil))
  (:documentation "This node contains a list of nodes that will be checked in sequence for addition or searching of components."))

;; outer interface
(defgeneric set-page-handler (node path handler &rest rest)
  (:documentation "Sets the page-handler for the subnode of the current node that matches with the given path"))
(defgeneric page-handler (node path &rest rest)
  (:documentation "Gets the page handler for the given path"))
(defgeneric page-url (node handler &rest rest)
  (:documentation "Gets a valid url for the given handler"))
(defgeneric tableize (node &rest rest)
  (:documentation "Creates a lisp-list from the given node that describes the workings of the current node"))

;; internal workings
(defgeneric handles-path (node path &rest rest)
  (:documentation "Defines whether or not the given path is handled by the given node."))
(defgeneric add-dispatched-node (node path handler &rest rest)
  (:documentation "Adds a subnode to the given node, that handles each of the sub-parts of the query separately, and connects the handler to the most specific node in the list."))
(defgeneric split-path (node path &rest rest)
  (:documentation "Splits the given path in separate pieces, defined by the rules of the given node."))

;; fixed-path-node
(defmethod split-path ((node fixed-path-node) (path string) &rest rest)
  (declare (ignore rest))
  (apply 'values (cl-ppcre:split (string +URLSPLIT+) path :limit 2)))

(defmethod add-dispatched-node ((node fixed-path-node) (path string) handler &rest rest)
  (declare (ignore rest))
  (let ((new-node (make-instance 'fixed-path-node
				 :base (split-path node path))))
    (set-page-handler new-node path handler) ; set for the whole path, as the first part will be stripped by the setting of the path itself
    (push new-node (subnodes node))))

(defmethod handles-path ((node fixed-path-node) path &rest rest)
  (declare (ignore rest))
  (when (string= (split-path node path) (base node))
    (split-path node path)))

(defmethod set-page-handler ((node fixed-path-node) (path string) handler &rest rest)
  (multiple-value-bind (current-path rest-path)
      (handles-path node path)
    (when current-path
      (if (not rest-path)
	  (setf (handler node) handler)
	  (block dispatched-handler
	    (dolist (subnode (subnodes node))
	      (when (handles-path subnode rest-path)
		(set-page-handler subnode rest-path handler)
		(return-from dispatched-handler T)))
	    (apply 'add-dispatched-node node rest-path handler rest))))))

(defmethod page-handler ((node fixed-path-node) (path string) &rest rest)
  (multiple-value-bind (current-path rest-path)
      (handles-path node path)
    (when current-path
      (if rest-path
	  (loop named handler-finder
	       for subnode in (subnodes node) do
	       (let ((handler (apply 'page-handler subnode rest-path rest)))
		 (when handler (return-from handler-finder handler))))
	  (handler node)))))

(defmethod page-url ((node fixed-path-node) handler &rest rest)
  (if (eql (handler node) handler)
      (base node)
      (loop named url-finder
	   for subnode in (subnodes node) do
	   (let ((url (apply 'page-url subnode handler rest)))
	     (when url
	       (return-from url-finder
		 (concatenate 'string (base node) (string +URLSPLIT+) url)))))))

(defmethod tableize ((node fixed-path-node) &rest rest)
  (declare (ignore rest))
  `(,(base node) ,(handler node) ,@(loop for x in (subnodes node) collect (tableize x))))


;; grouping node
(defmethod set-page-handler ((node grouping-node) (path string) handler &rest rest)
  (dolist (subnode (subnodes node))
    (when (handles-path subnode path)
      (set-page-handler subnode path handler)
      (return-from set-page-handler T)))
  ;; lets use a fixed-path-node by standard
  (let ((new-node (make-instance 'fixed-path-node)))
    (setf (base new-node) (split-path new-node path))
    (apply 'set-page-handler new-node path handler rest)
    (push new-node (subnodes node)))
  T)

(defmethod page-handler ((node grouping-node) path &rest rest)
  (dolist (subnode (subnodes node))
    (let ((handler (apply 'page-handler subnode path rest)))
      (when handler
	(return-from page-handler handler)))))

(defmethod page-url ((node grouping-node) handler &rest rest)
  (dolist (subnode (subnodes node))
    (let ((url (apply 'page-url subnode handler rest)))
      (when url
	(return-from page-url url)))))
  
(defmethod tableize ((node grouping-node) &rest rest)
  (map 'list (lambda (node) (apply 'tableize node rest)) (subnodes node)))


;; simplest outside interface
(defun clear-table ()
  (setf *table* (make-instance 'grouping-node)))
(clear-table)

(defun get-page-handler (path)
  (page-handler *table* path))
(defun get-page-url (page)
  (page-url *table* page))
(defun add-page (path handler)
  (set-page-handler *table* path handler))
(defun print-table ()
  (tableize *table*))