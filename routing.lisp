(defpackage :minions.routing
  (:use :common-lisp
	:cl-ppcre))

(in-package :minions.routing)

;; The current idea is to work from the data structure the user has given us.  This means that that data structure will be the internal representation too.
;;
;; In order for this to work out nicely, the system will be built up in 6 stages.
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
;;  Pages with variables will be generatable. <eg: (get-page-url 'show-post 'post-id 126)>
;;
;; h2. stage 5
;;  Pages can contain abbreviations (this is the handles clause)
;;
;; h2. stage 6
;;  Pages can contain functionality to define new variables (or logging, or whatever code they want to be executed in the current scope).

(defconstant +URLSPLIT+ #\/)

(defvar *ROUTING-TABLE* nil)
(defvar *url-variables* nil)

(defun set-routing-table (content)
  (setf *ROUTING-TABLE* content))

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
      (let ((parts (cl-ppcre:split (string +URLSPLIT+) url)))
	(dolist (route *ROUTING-TABLE*)
	  (let ((handler (search-handler route parts)))
	    (when handler
	      (return-from page-handler handler)))))))

(defun search-handler (route url-sections)
  "Searches for a handler for the current page in the currently known urls."
  (with-local-parsing-environment
    (when (string= (first route) (first url-sections))
      ;; moving to an iterative solution
      (maplist
       (lambda (unhandled-routing-directives)
	 (let ((item (first unhandled-routing-directives)))
	   (cond ((or (symbolp item) (functionp item))
		  (when (null (rest url-sections))
		    (setf *url-variables* url-variables) ;; storing the currently valid fetched variables
		    (return-from search-handler item)))
		 ((and (listp item) (stringp (first item)))
		  (let ((handler (search-handler item (rest url-sections))))
		    (when handler
		      (return-from search-handler handler))))
		 ((and (listp item) (eq (first item) 'handler))
		  (apply (second item) (first url-sections) (rest (rest item)))
		  (util:return-when search-handler (search-handler `(,(first route) ,@(rest unhandled-routing-directives)) url-sections))))))
       (rest route)))))

(defun handler-url (page &rest options)
  "Finds the url for a given page-handler.  The options can specify any number of (currently unspecified options)"
  (dolist (route *ROUTING-TABLE*)
    (let ((url (apply 'search-url route page options)))
      (when url
	(return-from handler-url url)))))

(defun search-url (route page &rest options)
  "Searches for a url of the given page, for the current (sub) route."
  (if (eql (second route) page)
      (first route)
      (dolist (subroute (rest route))
	(when (listp subroute)
	  (let ((url (apply 'search-url subroute page options)))
	    (when url
	      (return-from search-url (concatenate 'string (first route) (string +URLSPLIT+) url))))))))

(defun sets (url-part variable)
  "Handler sets ::
     Sets the currently matched url-part to the given variable."
  (declare (special url-variables))
  (push (cons variable url-part) url-variables))

(defun url-var (variable)
  "Returns the value of the variable found in the url."
  (cdr (assoc variable *url-variables*)))
	   
		 
;; (set-routing-table
;;  ("admin"
;;   'site.admin:overview
;;   ("users"
;;    'site.admin.users:index
;;    ("" 'site.admin.users:index)
;;    ("\d+" (handler identifies 'backend.users:user *user* by :id)
;; 	  (handles 'site.admin.users:show when (minions:request-method :get))
;; 	  (handles 'site.admin.users:create when (minions:request-method :put))
;; 	  (handles 'site.admin.users:destroy when (minions:request-method :destroy))
;; 	  ("edit" 'site.admin.users:edit when (minions:request-method :get)
;; 		  'site.admin.users:update when (minions:request-method :put))
;; 	  ("new" 'site.admin.users:new when (minions:request-method :get)
;; 		 'site.admin.users:new when (mininos:request-method :put))))
;;   ("tickets"
;;    (handles 'site.ticketing:index loosely)
;;    ("" 'site.ticketing:index)
;;    (".*" (handler identifies 'backend.ticketing by :title)))
;;   ("settings"
;;    (handles 'site.config:index loosely)
;;    ("config" (handles 'site.config:edit when (minions:request-method :get))
;; 	     (handles 'site.config:commit when (minions:request-method :put))))))
