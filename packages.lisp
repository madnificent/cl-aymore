(defpackage :util
  (:use :common-lisp)
  (:export :debug-print
	   :return-when))

(defpackage :minions
  (:use :cl :hunchentoot)
  (:export :defpage
	   :page-path
	   :param))

(defpackage :minions.routing
  (:use :common-lisp
	:hunchentoot
	:cl-ppcre)
  (:export :set-routing-table
	   :*static-files-path*
	   :*url-variables*
	   :url-variables
	   :page-handler
	   :handler-url
	   :defhandler
	   :handler
	   :only-when
	   :always
	   :never
	   :sets
	   :url-var
	   :defhandles
	   :loosely
	   :subroute))
	   
(defpackage minions.html
  (:use :common-lisp
	:minions
	:minions.routing)
  (:export :html 
	   :head 
	   :title 
	   :body 
	   :h1 
	   :h2 
	   :h3 
	   :h4 
	   :h5 
	   :h6
	   :table :tr :td
	   :p 
	   :div 
	   :ul
	   :ol
	   :li
	   :span 
	   :strong
	   :em
	   :b :i
	   :a
	   :br
	   :form
	   :input
	   :textarea
	   :style
	   :hidden)
  (:export :htmlify
	   :button-to
	   :bttn-to
	   :link-to-page
	   :redirect-to-page
	   :build-path))