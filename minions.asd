(defpackage :minions.sysdef
  (:use :common-lisp :asdf))

(in-package :minions.sysdef)

(defsystem :minions
  :name "Minions - Base definition"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "BSD"
  :description "Website definition system"
  :depends-on (:hunchentoot
	       :cl-ppcre)
  :components ((:file "packages")
	       (:file "helpers")
	       (:file "minions")
	       (:file "routing" :depends-on ("minions" "helpers"))
	       (:file "html" :depends-on ("minions"))))