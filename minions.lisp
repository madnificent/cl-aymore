(defpackage :minions
  (:use :cl :hunchentoot)
  (:export :defpage))
(in-package :minions)

(defmacro defpage (name path &body content)
  "Defines a page, and adds it to the dispatch-table"
  (let ((statements nil))
    (when (listp (first content)) ;; define the function if it is not a key
      (push `(defun ,name () ,@content)
	    statements))
    ;; add the route
    (push `(push (create-prefix-dispatcher ,path ',name)
		 *dispatch-table*)
	  statements)
    `(progn ,@(reverse statements))))
