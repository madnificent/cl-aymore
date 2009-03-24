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
  (:export :a ;; strict xhtml tags
	   :abbr
	   :acronym
	   :address
	   :area
	   :b
	   :base
	   :bdo
	   :big
	   :blockquote
	   :body
	   :br
	   :button
	   :caption
	   :cite
	   :code
	   :col
	   :colgroup
	   :dd
	   :del
	   :div
	   :dfn
	   :dl
	   :dt
	   :em
	   :fieldset
	   :form
	   :h1
	   :head
	   :hr
	   :html
	   :i
	   :img
	   :input
	   :ins
	   :kbd
	   :label
	   :legend
	   :li
	   :link
	   :image-map ; yes, this is somewhat dirty, it is actually the map tag
	   :meta
	   :noscript
	   :object
	   :ol
	   :optgroup
	   :option
	   :p
	   :param
	   :pre
	   :q
	   :samp
	   :script
	   :select
	   :small
	   :span
	   :strong
	   :style
	   :sub
	   :sup
	   :table
	   :tbody
	   :td
	   :textarea
	   :tfoot
	   :th
	   :thead
	   :title
	   :tr
	   :tt
	   :ul
	   :var)
  (:export :htmlify
	   :button-to
	   :bttn-to
	   :link-to-page
	   :redirect-to-page
	   :build-path
	   :text-field
	   :text-area
	   :submit-button))