(defpackage :CLaymore.html.tests
  (:use :common-lisp
	:CLaymore.html
	:cl-ppcre
	:stefil)
  (:export :html-tests))

(defpackage :CLaymore.routing.tests
  (:use :common-lisp
	:CLaymore.routing
	:stefil)
  (:export :routing-tests))