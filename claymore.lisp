(in-package :CLaymore)

(defparameter *lookup-table* nil)

(defmacro defpage (name vars &body content)
  "Defines a page, and adds it to the dispatch-table"
  `(defun ,name (&key ,@(loop for var in vars collect `(,var (param* ',var))))
     ,@content))

(defun param (variable)
  "Returns the value of the given variable of the last request"
  (or (claymore.routing:url-var variable) 
      (parameter (if (stringp variable) variable (format nil "~A" variable)))
      (parameter (if (stringp variable) variable (string-downcase (format nil "~A" variable)))))) ;; try lowercased if it the upcase doesn't work

(defun param* (variable)
  "Returns the value of the given variable of the last request or of the session"
  (or (param variable)
      (hunchentoot:session-value variable)
      (hunchentoot:session-value (format nil "~A" variable))
      (hunchentoot:session-value (string-downcase (format nil "~A" variable))))) ;; try lowercased if it the upcase doesn't work