(defpackage :minions.tests.sysdef
  (:use :common-lisp :asdf))

(in-package :minions.tests.sysdef)

(defsystem :minions.tests
  :name "Minions - tests"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "BSD"
  :description "Website definition system"
  :depends-on (:minions :stefil)
  :components ((:file "packages")
	       (:file "html"
		      :depends-on ("packages"))
	       (:file "routing"
		      :depends-on ("packages"))))