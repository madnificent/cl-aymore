(defpackage :minions
  (:use :cl :hunchentoot)
  (:export :defpage))
(in-package :minions)

(defmacro defpage (path &rest content)
  (let ((function (if (listp (first content))
		      `(lambda () ,@content)
		      `(quote ,(first content)))))
    `(push (create-prefix-dispatcher ,path ,function) *dispatch-table*)))
      