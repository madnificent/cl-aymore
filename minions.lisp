(in-package :minions)

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

(defmacro defpage (name &body content)
  "Defines a page, and adds it to the dispatch-table"
  `(defun ,name ()
     ,@content))

(defun param (variable)
  "Returns the value of the given variable of the last request"
  (if (stringp variable)
      (parameter variable)
      (parameter (format nil "~A" variable))))