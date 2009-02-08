;;;; This package contains some handy utilities
(in-package :util)

(defmacro debug-print (&rest vars)
  (let ((formats (mapcar (lambda (var) `(format T "~& ~A = ~A~%" (quote ,var) ,var)) vars)))
    `(progn ,@formats)))

(defmacro return-when (return-to &body body)
  (let ((temp (gensym)))
    `(let ((,temp (progn ,@body)))
       (when ,temp
	 (return-from ,return-to ,temp)))))