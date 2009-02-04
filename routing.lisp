(defpackage :minions.routing
  (:use :common-lisp
	:hunchentoot
	:cl-ppcre)
  (:export :set-routing-table
	   :page-handler
	   :handler-url
	   :defhandler
	   :sets
	   :url-var
	   :defhandles
	   :loosely))
	   
(in-package :minions.routing)

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
;;  Operations for managing the routing table.  These operations are only basic, as the more complicated behavior can be done manually (readable-table) gives you a table that is editable.  Manipulating that list (which is easy) allows the system to be reconfigured based on the user's needs.
;; example:
;;  (move "foo/bar" "foo/baz")
;;  (delete "foo/bar")
;;  (insert "foo/bar" (handles bar loosely) ("search" 'search))
;;  (update "foo/bar" (subtable-list)) ; this is a delete and an insert
;;  (readable-table)
;;
;; h2. stage 8
;;  Named subsections.  These named subsections can be updated without direct table manipulation.  This would provide better support for creating plugins that contain routes to manage the plugins.
;; example:
;;  (insert "foo/bar" 'posts-routing)
;;  (update 'posts-routing '("posts" (handles post-list loosely) ("\\d+" (handler sets post-id) (when get-request show-post))))

(defconstant +URLSPLIT+ #\/)

(defvar *ROUTING-TABLE* nil)
(defvar *url-variables* nil)

(defun set-routing-table (content)
  (setf *ROUTING-TABLE* (expand-handles-cases content))
  (setf hunchentoot:*dispatch-table* 
	(list (create-prefix-dispatcher "" 'hunchentoot-get-handler))))

(defun hunchentoot-get-handler ()
  (funcall (page-handler (first (cl-ppcre:scan-to-strings "$[^\\?]+" (hunchentoot:request-uri))))))

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
      (let ((parts (cl-ppcre:split (string +URLSPLIT+) url)))
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
	   (cond ((or (symbolp item) (functionp item))
		  (when (null (rest url-sections))
		    (setf *url-variables* url-variables) ;; storing the currently valid fetched variables
		    (return-from search-handler item)))
		 ((and (listp item) (stringp (first item)))
		  (let ((handler (search-handler item (rest url-sections))))
		    (when handler
		      (return-from search-handler handler))))
		 ((and (null (rest url-sections)) (listp item) (eq (first item) 'when))
		  (util:return-when search-handler (apply (funcall (second item) :assert) (rest (rest item)))))
		 ((and (listp item) (eq (first item) 'handler))
		  (apply (funcall (second item) :to-page) (first url-sections) (rest (rest item)))
		  (util:return-when search-handler (search-handler `(,(first route) ,@(rest unhandled-routing-directives)) url-sections))))))
       (rest route)))))

(defun handler-url (page &rest options)
  "Finds the url for a given page-handler.  The options can specify any number of (currently unspecified options)"
  (dolist (route *ROUTING-TABLE*)
    (let ((url (apply 'search-url route page nil options)))
      (when url
	(return-from handler-url (format nil "/~{~a~^/~}" (reverse url)))))))

(defun search-url (route page url-sections &rest options)
  "Searches for a url of the given page, for the current (sub) route."
  (let ((url-sections (cons (first route) url-sections))) ;; last part of the url is the first item in this list
    (dolist (item (rest route))
      (cond ((or (symbolp item) (functionp item))
	     (when (eql page item)
	       (return-from search-url url-sections)))
	    ((and (listp item) (stringp (first item)))
	     (util:return-when search-url (apply 'search-url item page url-sections options)))
	    ((and (listp item) (eq (first item) 'when))
	     (when (eql (apply (funcall (second item) :enforce) (rest (rest item))) page)
	       (return-from search-url url-sections)))
	    ((and (listp item) (eq (first item) 'handler))
	     (let ((new-url-sections (apply (funcall (second item) :to-url) (rest (rest item)))))
	       (if new-url-sections
		   (setf url-sections new-url-sections)
		   (return-from search-url nil))))))))

(defmacro defhandles (name (base &rest args) documentation &body body)
  "Allows users to create a new clause for the handles expanders.
   This will receive some content and must then return a new list, which will be spliced into the given routing table."
  `(defun ,name (,base ,@args)
     ,documentation
     ,@body))

(defmacro defwhen (name documentation ((&rest assert-args) &body assert-body) ((&rest enforce-args) &body enforce-body))
  "Allows users to create new conditionally allowed parts in the routing.  The current system is not allowed to change the URLs in any way, this may be subject to change."
  (let ((dir (gensym)))
    `(defun ,name (,dir)
       ,documentation
       (cond ((eql ,dir :assert)
	      (lambda ,assert-args
		,@assert-body))
	     ((eql ,dir :enforce)
	      (lambda ,enforce-args
		,@enforce-body))))))

(defmacro defhandler (name documentation ((&rest to-page-args) &body to-page-body) ((&rest to-url-args) &body to-url-body))
  (let ((dir (gensym)))
    `(defun ,name (,dir)
       ,documentation
       (cond ((eql ,dir :to-page)
	      (lambda ,to-page-args
		,@to-page-body))
	     ((eql ,dir :to-url)
	      (lambda ,to-url-args
		,@to-url-body))))))

(defwhen always
    "Simple when-clause that may be executed in any case"
  ((item) item)
  ((item) item))
(defwhen never
    "Simple when-clause that may never be executed"
  ((item) (declare (ignore item)) nil)
  ((item) (declare (ignore item)) nil))

(defhandles loosely (page)
    "Allows a page to be linked both through foo and foo/, with foo being the predefined regexp in the routing table"
  `(,page ("" ,page)))

(defhandles only-when (page condition)
    "Converts (handles page when condition) to (when condition page), for those that would like one definition above the other."
  `((when ,condition ,page)))

(defhandler sets
    "Sets the currently matched url-part to the given variable"
  ((url-part variable)
   (declare (special url-variables))
   (push (cons variable url-part) url-variables))
  ((url-sections options variable)
   (when (getf options variable)
     (cons (getf options variable) (rest url-sections)))))

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
