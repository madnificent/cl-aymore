(defpackage :minions
  (:use :cl :hunchentoot)
  (:export :defpage))
(in-package :minions)

(defmacro defpage (path content)
  (let ((function (gensym)))
    `(let ((,function (if (symbolp ,content)
			  ,content
			  (lambda () ,content))))
       (push (create-prefix-dispatcher ,path ,function) *dispatch-table*))))
      