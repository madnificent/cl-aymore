(defpackage :CLaymore.tests.sysdef
  (:use :common-lisp :asdf))

(in-package :CLaymore.tests.sysdef)

(defsystem :CLaymore.tests
  :name "CLaymore - tests"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "BSD"
  :description "Website definition system"
  :depends-on (:CLaymore :stefil)
  :components ((:file "packages")
	       (:file "html"
		      :depends-on ("packages"))
	       (:file "routing"
		      :depends-on ("packages"))))