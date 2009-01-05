(defpackage :site.requests
  (:use :common-lisp
	:minions
	:minions.html)
  (:documentation
"This package resembles a site that has the ability to the requests (with somewhat detailed info) of the users that connected to it.  It is mainly an example as to be able to show how the new page-linking system works.")
  (:export :index
	   :list-requests
	   :*requests*))

(in-package :site.requests)

(defvar *requests* nil)

(defun register-request ()
  "Stores the current request"
  (declare (special hunchentoot:*request*))
  (push hunchentoot:*request* *requests*))

(defun bordered-div (class &rest body)
  "Shows a div with a red border surrounding it."
  (div :class class :style "border-style:solid;border-width:1px;border-color:red;"
       body))

(defun links-pane ()
  "Shows a div with all the links for the page in it."
  (bordered-div "links" (h2 "links")
		(ul (li (link-to-page "home" 'index))
		    (li (link-to-page "requests" 'list-requests)))))
		
(defun standard-page (title &rest body)
  (register-request)
  (html (head (title "minions :: " title))
	(body (bordered-div "header" (h1 title))
	      (links-pane)
	      body)))

(defpage index "/index.html"
  (standard-page 
   "Hello minions"
   (p "Hello my dear minions, this is a page generated with the minions-system.  It shows some basic useage of the linking system, which will allow us to create a simple log of the pages that were visited.  True, this is somewhat useless, but it does allow new users te easily grasp the way we can easily deal with ready-made objects in this system.")
   (p "For this to run quickly, we are using the request-objects that hunchentoot gives us.  Although this is not something you must grasp in order to be able to work with minions, it should allow you to see how everything works.")
   (p "When you look into the source, you may find such statements as hunchentoot:foo , with foo being a keyword in the hunchentoot package.  All those statements are specific to the hunchentoot system.")))

(defpage list-requests "/requests/index.html"
  (labels ((show-request (request)
	     (li (hunchentoot:remote-addr request) (hunchentoot:request-uri request))))
    (standard-page 
     "Requests"
     (ul (loop for request in *requests* collect (show-request request))))))

(defpage show-request "/requests/show.html"
  (let ((request (elt (reverse *requests*) (read-from-string (param "id")))))
    (standard-page
     "Request"
     (ul (li (strong "remote-addr") " : " (hunchentoot:remote-addr request))
	 (li (strong "request-uri") " : " (hunchentoot:request-uri request))))))