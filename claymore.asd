(defpackage :CLaymore.sysdef
  (:use :common-lisp :asdf))

(in-package :CLaymore.sysdef)

(defsystem :claymore
  :name "CLaymore - Base definition"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "BSD"
  :description "Common Lisp Agile yaddayadda multipurpose online routable environment -- aka Website definition system"
  :depends-on (:hunchentoot
	       :cl-ppcre)
  :components ((:file "packages")
	       (:file "helpers" :depends-on ("packages"))
	       (:file "claymore" :depends-on ("packages"))
	       (:file "routing" :depends-on ("claymore" "helpers" "packages"))
	       (:file "routing.extensions" :depends-on ("routing"))
	       (:file "html" :depends-on ("claymore" "packages"))
	       (:file "html.extensions" :depends-on ("html"))))