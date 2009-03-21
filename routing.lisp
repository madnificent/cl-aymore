(in-package :CLaymore.routing)

;; The current idea is to work from the data structure the user has given us.  This means that that data structure will be the internal representation too.
;;
;; In order for this to work out nicely, the system will be built up in 7 stages.
;; h2. stage 1
;;  Pages will be identifyable by the internal representation.  Only static pages will be considered.  That means that we can render a page from the hunchentoot.
;;  example
;; (set-routing-table '(("about" about-overview
;;                       ("contact" contact)
;;                       ("overview" overview)
;;                       ("status" status))
;;                      ("welcome" welcome)))
;; (page-handler "about/contact") >> 'contact
;;
;; h2. stage 2
;;  The url for a page will be creatable, based on the page (and possible some options). This will be for non-regexp pages.
;;  example
;; (handler-url 'contact) >> "about/contact"
;;
;; h2. stage 3
;;  Pages will be able to parse the content in a URL, and this will allow them to define variables that the user can access.
;; (set-routing-table '(("posts" posts)
;;                       ("show"
;;                        ("\\d+" (handler sets post-id)
;;                                show-post)))))
;; (defpage show-post () (with-standard-page (h1 "Requested : " (url-var 'post-id))))
;;
;; h2. stage 4
;;  Pages with variables will be generatable.
;; example:
;;  (page-handler 'show-post 'post-id 126)
;;
;; h2. stage 5
;;  Pages can contain abbreviations (this is the handles clause)
;; example:
;;  (set-routing-table '(("posts" (handles posts loosely)))
;;  expands to
;;  (set-routing-table '(("posts" posts
;;                        ("" posts))))
;;
;; h2. stage 6
;;  Pages can define conditional evaluation (when (condition) action).  Both (when ...) and (handles action when (condition)) should work.
;; example:
;;  (set-routing-table '(("posts" (handles post-list loosely)
;;                        ("\\d+" (handler sets post-id)
;;                         (when get-request show-post)
;;                         (when put-request create-post)
;;                         (when destroy-request destroy-post)))
;;                       ("users" (handles users loosely)
;;                        ("\\d+" (handler sets user-id)
;;                         (handles show-user when get-request)
;;                         (handles update-user when post-request)))))
;;
;; h2. stage 7
;;  Named subsections.  These named subsections can be updated without direct table manipulation.  This would provide better support for creating plugins that contain routes to manage the plugins.
;; example:
;;  (defsubroute users (handles users loosely)
;;                     ("\\d+" (handler sets user-id)
;;                      (handles show-user when get-request)
;;                      (handles update-user when post-request))))
;;  (set-routing-table '(index
;;                       (subroute users for "users")))))
;;
;; h2. stage 8 (currently unimplemented)
;;  Operations for managing the routing table.  These operations are only basic, as the more complicated behavior can be done manually (readable-table) gives you a table that is editable.  Manipulating that list (which is easy) allows the system to be reconfigured based on the user's needs.
;; example:
;;  (move "foo/bar" "foo/baz")
;;  (delete "foo/bar")
;;  (insert "foo/bar" (handles bar loosely) ("search" 'search))
;;  (update "foo/bar" (subtable-list)) ; this is a delete and an insert
;;  (readable-table)

(defconstant +URLSPLIT+ #\/)

(defvar *ROUTING-TABLE* nil)
(defvar *STATIC-DIRECTORIES* nil)
(defvar *subroutes* (make-hash-table))

;;;; Stuff that needs cleaning up
(defun set-hunchentoot-routing-table ()
  "Sets hunchentoots routing table, so it adheres to the data we set"
  (let ((hunchentoot-routes (list (create-prefix-dispatcher "" 'hunchentoot-get-handler)))) ; the explicit routing defined by claymore.routing
    (loop for (base-path settings) on *STATIC-DIRECTORIES* by #'cddr do
	 (push (apply 'hunchentoot:create-folder-dispatcher-and-handler `(,base-path ,@settings))
	       hunchentoot-routes))
    (setf hunchentoot:*dispatch-table* hunchentoot-routes)))
(set-hunchentoot-routing-table)

(defun add-static-routing-dispatcher (uri-prefix base-path &optional content-type)
  "Adds a handling system for some static content"
  (setf (getf *STATIC-DIRECTORIES* uri-prefix)
	(list base-path content-type))
  (set-hunchentoot-routing-table))

(defun set-routing-table (routes)
  "Sets the routing table to the given route"
  (setf *ROUTING-TABLE* (expand-handles-cases (list (concatenate 'list '("") routes)))))

(defun hunchentoot-get-handler ()
  (declare (special hunchentoot:*request*))
  (let ((func (page-handler (cl-ppcre:scan-to-strings "[^\\?]+" (hunchentoot:request-uri hunchentoot:*request*)))))
    (when func
      (funcall func))))

;;;; subroute definition
(defun subroute (name)
  "Returns the subroute defined by <name>"
  (expand-handles-cases (gethash name *subroutes*)))
(defun set-subroute (name route)
  (setf (gethash name *subroutes*) route))
(defsetf subroute set-subroute)

;;;; Routing system
(defun expand-handles-cases (content)
  (let ((resulting-list))
    (mapc (lambda (item)
	    (cond ((and (listp item) 
			(eql (first item) 'handles))
		   (setf resulting-list 
			 (reverse 
			  (expand-handles-cases 
			   (reverse`(,@(reverse (apply (third item) (second item) (rest (rest (rest item))))) ,@resulting-list))))))
		  ((listp item)
		   (push (expand-handles-cases item) resulting-list))
		  (T
		   (push item resulting-list))))
	  content)
    (reverse resulting-list)))

(defmacro with-parsing-environment (&body body)
  "Sets up the environment for the parsing of the url.  This will give you the basic space to store variables in etc."
  `(let ((url-variables nil))
     (declare (special url-variables))
     ,@body))

(defmacro with-local-parsing-environment (&body body)
  "Sets up a local parsing environment.  This mustn't be called outside of the body of with-parsing-environment."
  `((lambda ()
      (declare (special url-variables))
      (let ((url-variables (copy-list url-variables)))
	(declare (special url-variables))
	,@body))))

(defun page-handler (url)
  "Finds the handler for a page in the current routing table."
  (with-parsing-environment
    (let ((parts (map 'list 'hunchentoot:url-decode (cl-ppcre:split (string +URLSPLIT+) url))))
      (dolist (route *ROUTING-TABLE*)
	(let ((handler (search-handler route parts)))
	  (when handler
	    (return-from page-handler handler)))))))

(defun search-handler (route url-sections)
  "Searches for a handler for the current page in the currently known urls."
  (with-local-parsing-environment
    (when (cl-ppcre:scan (concatenate 'string "^" (first route) "$") (first url-sections))
      ;; moving to an iterative solution
      (maplist
       (lambda (unhandled-routing-directives)
	 (let ((item (first unhandled-routing-directives)))
	   (cond ( ;; handler-function
		  (or (symbolp item) (functionp item))
		  (when (null (rest url-sections))
		    (setf *url-variables* url-variables) ;; storing the currently valid fetched variables (should be done in some sort of handler)
		    (return-from search-handler item)))
		 ( ;; inline subroute
		  (and (listp item) (stringp (first item)))
		  (let ((handler (search-handler item (rest url-sections))))
		    (when handler
		      (return-from search-handler handler))))
		 ( ;; subroute case
		  (and (listp item) (eq (first item) 'subroute))
		  (util:return-when search-handler (search-handler `(,(fourth item) ,@(subroute (second item))) (rest url-sections))))
		 ( ;; when case
		  (and (and (listp item) (eq (first item) 'when)))
		  (let ((new-route (apply (funcall (second item) :assert) (rest (rest item)))))
		    (when new-route
		      (util:return-when search-handler 
			(search-handler `(,(first route) ,@new-route)
					url-sections)))))
		 ( ;; handler case
		  (and (listp item) (eq (first item) 'handler))
		  (if (apply (funcall (second item) :to-page) (first url-sections) (rest (rest item)))
		      (util:return-when search-handler (search-handler `(,(first route) ,@(rest unhandled-routing-directives)) url-sections))
		      (return-from search-handler nil))))))
       (rest route))
      nil)))

(defun handler-url (page &rest options)
  "Finds the url for a given page-handler.  The options can specify any number of (currently unspecified options)"
  (dolist (route *ROUTING-TABLE*)
    (let ((url (search-url route page nil options)))
      (when url
	(return-from handler-url
	  (if (equal '("") url)
	      "/"
	      (let ((foo (format nil "~{~A~^/~}" (reverse (map 'list 'hunchentoot:url-encode url)))))
		(util:debug-print foo)
		foo)))))))
	    

(defun search-url (route page url-sections options)
  "Searches for a url of the given page, for the current (sub) route."
  (let ((url-sections (cons (first route) url-sections))) ;; last part of the url is the first item in this list
    (dolist (item (rest route))
      (cond ( ;; handler function
	     (or (symbolp item) (functionp item))
	     (when (eql page item)
	       (return-from search-url url-sections)))
	    ( ;; inline subroute
	     (and (listp item) (stringp (first item)))
	     (util:return-when search-url (search-url item page url-sections options)))
	    ( ;; subroute case
	     (and (listp item) (eq (first item) 'subroute))
	     (util:return-when search-url (search-url `(,(fourth item) ,@(subroute (second item))) page url-sections options)))
	    ( ;; when case
	     (and (listp item) (eq (first item) 'when))
	     (let ((new-route (apply (funcall (second item) :enforce) (rest (rest item)))))
	       (util:return-when search-url
		 (search-url `(,(first route) ,@new-route) page (rest url-sections) options))))
;	     (when (eql (apply (funcall (second item) :enforce) (rest (rest item))) page)
;	       (return-from search-url url-sections)))
	    ( ;; handler case
	     (and (listp item) (eq (first item) 'handler))
	     (let ((new-url-sections (funcall (funcall (second item) :to-url) url-sections options (rest (rest item)))))
	       (if new-url-sections
		   (progn
		     (util:debug-print url-sections new-url-sections)
		     (setf url-sections new-url-sections))
		   (return-from search-url nil))))))))