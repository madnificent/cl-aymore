(in-package :sysdef-user)
(define-system :minions ()
  (:author "Aad versteden")
  (:version 0 0 1)
  (:licence "MIT Style license")
  (:documentation "Minions is a user-defined webframework to quicken the development of easy to maintain applications.")
  (:components
   "minions"
   "html")
  (:needs :hunchentoot))
