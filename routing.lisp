(defpackage :minions.routing
  (:use :common-lisp
	:cl-ppcre)
  ;; handlers on node-objects
  (:export :set-page-handler
	   :page-handler
	   :page-url
	   :tableize)
  ;; handlers on the general system
  (:export :clear-table
	   :get-page-handler
	   :get-page-url
	   :add-page
	   :print-table))

(in-package :minions.routing)

(defvar *table* nil)
(defvar *URL-VARS* nil)


;; The current idea is to work from the data structure the user has given us.  This means that that data structure will be the internal representation too.
;;
;; In order for this to work out nicely, the system will be built up in 6 stages.
;; h2. stage 1
;;  Pages will be identifyable by the internal representation.  Only static pages will be considered.  That means that we can render a page from the hunchentoot.
;;  example
;; (set-routing-table '(("about" 'about-overview
;; 		      ("contact" 'contact)
;; 		      ("overview" 'overview)
;; 		      ("status" 'status))
;; 		     ("welcome" 'welcome)))
;; h2. stage 2
;;  The url for a page will be creatable, based on the page (and possible some options). This will be for non-regexp pages.
;; h2. stage 3
;;  Pages will be able to parse the content in a URL, and this will allow them to define variables that the user can access.
;; h2. stage 4
;;  Pages with variables will be generatable. <eg: (get-page-url 'show-post :post-id 126)>
;; h2. stage 5
;;  Pages can contain abbreviations (this is the handles clause)
;; h2. stage 6
;;  Pages can contain functionality to define new variables (or logging, or whatever code they want to be executed in the current scope).

(defconstant +URLSPLIT+ #\/)

(defvar *ROUTING-TABLE* nil)

