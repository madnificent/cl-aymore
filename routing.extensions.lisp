(in-package :CLaymore.routing)

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

;; when-handlers
(defwhen post-request
    "Evaluates when the current request was a post-request"
  ((&rest items) 
   (when (eq (hunchentoot:request-method*) :post)
     items))
  ((&rest items)
   (warn 'simple-warning :format-control "Creating a url for a POST-request, even though I have no idea whether or not it is one.~%")
   items))

(defwhen get-request
    "Evaluates when the current request was a get-request"
  ((&rest items)
   (when (eq (hunchentoot:request-method*) :get)
     items))
  ((&rest items)
   (warn 'simple-warning :format-control "Creating a url for a GET-request, even though I have no idea whether or not it is one.~%")
   items))

(defwhen always
    "Simple when-clause that may be executed in any case"
  ((&rest items) items)
  ((&rest items) items))
(defwhen never
    "Simple when-clause that may never be executed"
  ((&rest items) (declare (ignore items)) nil)
  ((&rest items) (declare (ignore items)) nil))

;; handles expanders
(defhandles loosely (page)
    "Allows a page to be linked both through foo and foo/, with foo being the predefined regexp in the routing table"
  `(,page ("" ,page)))

(defhandles only-when (page condition)
    "Converts (handles page when condition) to (when condition page), for those that would like one definition above the other."
  `((when ,condition ,page)))

;; handler operations
(defvar *url-variables* nil)
(defvar *handles* (make-hash-table))

(defun url-var (variable)
  "Returns the value of the variable found in the url."
  (cdr (assoc variable *url-variables*)))

(defhandler sets
    "Sets the currently matched url-part to the given variable"
  ((url-part variable)
   (declare (special url-variables))
   (push (cons variable url-part) url-variables))
  ((url-sections options variables)
   (let ((var (first variables)))
     (when (getf options var)
       (cons (getf options var) (rest url-sections))))))

(defmacro def-identification-handler (name object->url-section url-section->object)
  "This creates an 'identifies <var> as <value>' handler that can be used in routes.
In essence, this allows you to create a simple mapping from a data-structure to a fully qualified name that can then be used in the url.
The general idea is to allow you to say (def-identification-handler user 'nick 'find-user-by-nick) to add this handler to the handlers currently known by the handler identifies.  
Which will make the url understand (handler identifies user as my-variable).  That will set the the my-variable as a url-variable to the value of the user."
  (let ((gname (gensym)))
    `(let ((,gname ',name))
       (setf (identifies-function ,gname :o->u) ,object->url-section)
       (setf (identifies-function ,gname :u->o) ,url-section->object))))

(defun identifies-function (name dir)
  "Returns the functions that will be used by the identifies handler for the given name and direction.
This is a setfable place, but you may prefer to use the def-identification-handler macro for that."
  (getf (gethash name *handles*) dir))

(defun set-identifies-function (name dir value)
  "Sets the functions that will be used by the identifies handler for the given name and direction."
  (setf (getf (gethash name *handles*) dir) value))
(defsetf identifies-function set-identifies-function)

(defhandler identifies
    "Generic identification handler.  This can be extended by using the def-identification-handler macro."
  ((url-part name as variable)
   (declare (special url-variables)
	    (ignore as))
   (hunchentoot:log-message :info "Identifying ~A as ~A from ~A" name variable url-part)
   (let ((object (funcall (identifies-function name :u->o) url-part)))
     (when object
       (push (cons variable object) url-variables))))
  ((url-sections options statements)
   (let* ((object (getf options (third statements)))
	  (url-encoded-object (and object (funcall (identifies-function (first statements) :o->u) object))))
     (when url-encoded-object
       (cons url-encoded-object (rest url-sections))))))
	    