(defpackage :util
  (:use :common-lisp)
  (:export :debug-print
	   :return-when))

(defpackage :CLaymore
  (:use :cl :hunchentoot)
  (:export :defpage
	   :page-path
	   :param))

(defpackage :CLaymore.routing
  (:use :common-lisp
	:hunchentoot
	:cl-ppcre)
  (:export :set-routing-table
	   :add-static-routing-dispatcher
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
	   :defwhen
	   :handles
	   :loosely
	   :subroute
	   :post-request
	   :get-request
	   :identifies
	   :def-identification-handler
	   :identifies-function))
	   
(defpackage CLaymore.html
  (:use :common-lisp
	:CLaymore
	:CLaymore.routing)
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
	   :table :tr :td :tr
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
	   :hr
	   :form
	   :input
	   :textarea
	   :style
	   :hidden
	   :label
	   :img
	   :script
	   :link)
  (:export :htmlify
	   :button-to
	   :bttn-to
	   :link-to-page
	   :redirect-to-page
	   :build-path
	   :text-field
	   :text-area
	   :submit-button))