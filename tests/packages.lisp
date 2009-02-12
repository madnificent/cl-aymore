(defpackage :minions.html.tests
  (:use :common-lisp
	:minions.html
	:cl-ppcre
	:stefil)
  (:export :tests))

(defpackage :minions.routing.tests
  (:use :common-lisp
	:minions.routing
	:stefil)
  (:export :tests))