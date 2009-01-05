(defpackage :site.blog
  (:use :common-lisp
	:minions
	:minions.html)
  (:documentation
"This package provides you with a blog.  It shows you how you can easily use parameters.  It doesn't include the usage of formbuilders (for they are non-existant for now."))

(in-package :site.blog)

(defvar *posts* nil)

(defclass post ()
  ((owner :reader owner
	  :initarg :owner
	  :initform "anonymous coward")
   (ptitle :reader ptitle
	   :initarg :title
	  :initform "")
   (content :reader content
	    :initarg :content
	    :initform "")))

(defun store-post ()
  "Stores the post-object that has been given to us from the users."
  (push (make-instance 'post
		       :owner (param :owner)
		       :title (param :title)
		       :content (param :content))
	*posts*))

(defun bordered-div (class &rest body)
  "Shows a div with a red border surrounding it."
  (div :class class :style "border-style:solid;border-width:1px;border-color:red;"
       body))

(defun links-pane ()
  "Shows a div with all the links for the page in it."
  (bordered-div "links" (h2 "links")
		(ul (li (link-to-page "home" 'index))
		    (li (link-to-page "new post" 'add-post))
		    (li (link-to-page "clear" 'clear-posts)))))
		
(defun standard-page (title &rest body)
  (html (head (title "blog :: minions" title))
	(body (bordered-div (h1 "Minions blog " title))
	      (links-pane)
	      body)))

(defun show-post-summary (post)
  "Shows a single post"
  (bordered-div "post"
		(h2 (ptitle post))
		(strong (owner post))
		(bordered-div "content" (content post))))

(defpage index "/blog/"
  (standard-page 
   "overview"
   (div :class "content"
	(loop for post in *posts* collect
	     (show-post-summary post)))))

(defpage add-post "/blog/new"
  (standard-page
   "add post"
   (form :action (page-path 'commit-post) :method "post"
	 (strong "title : ") (input :type "text" :size 40 :name :title) (br)
	 (strong "author : ") (input :type "text" :size 20 :name :owner) (br)
	 (strong "content") (br) (textarea :name :content :cols 40 :cols 5 "Article content here.") (br)
	 (input :type "submit"))))

(defpage commit-post "/blog/commit"
  (store-post)
  (index)) ;; redirect would be sweeter

(defpage clear-posts "/blog/clear"
  (setf *posts* nil)
  (index)) ;; redirect would be sweeter