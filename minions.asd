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
	       (:file "helpers" :depends-on ("packages"))
	       (:file "minions" :depends-on ("packages"))
	       (:file "routing" :depends-on ("minions" "helpers" "packages"))
	       (:file "routing.extensions" :depends-on ("routing"))
	       (:file "html" :depends-on ("minions" "packages"))))