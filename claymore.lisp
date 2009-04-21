(in-package :CLaymore)

(defparameter *lookup-table* nil)

;; (defun register-page (name path)
;;   "Registers a page in the page-listing system.  After doing this, the page is recorded in the system, which allows the creations of links built by the name of the page."
;;   (if (assoc name *lookup-table*)
;;       (setf (cdr (assoc name *lookup-table*)) path)
;;       (push (cons name path) *lookup-table*))
;;   path)

;; (defmacro defpage (name path &body content)
;;   "Defines a page, and adds it to the dispatch-table"
;;   (let ((statements nil))
;;     (when (listp (first content)) ;; define the function if it is not a key
;;       (push `(defun ,name () ,@content)
;; 	    statements))
;;     ;; add the route to hunchentoot
;;     (push `(push (create-prefix-dispatcher ,path ',name)
;; 		 *dispatch-table*)
;; 	  statements)
;;     ;; add the route for links
;;     (push `(register-page ',name ,path) 
;; 	  statements)
;;     ;; return the statements
;;     `(progn ,@(reverse statements))))

(defmacro defpage (name vars &body content)
  "Defines a page, and adds it to the dispatch-table"
  `(defun ,name (&key ,@(loop for var in vars collect `(,var (param* ',var))))
     ,@content))

(defun param (variable)
  "Returns the value of the given variable of the last request"
  (or (claymore.routing:url-var variable) 
      (parameter (if (stringp variable) variable (format nil "~A" variable)))))

(defun param* (variable)
  "Returns the value of the given variable of the last request or of the session"
  (or (param variable)
      (hunchentoot:session-value variable)
      (hunchentoot:session-value (format nil "~A" variable))))