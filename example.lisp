(defpackage :my-site
  (:use :common-lisp
	:minions))

(in-package :my-site)

;; First example
(defpage "/hello.html"
    (html
     (head (title "Hello world"))
     (body (h1 "Hello, world!"))))

;; Functional example
(defun standard-page (title &rest page)
  (html 
   (head (title title))
   (body page)))

(defpage "/index.html"
    (standard-page "Simple example"
      (h1 "Functional!")
      (p "Good, we have functional content no!")))

;; RESTFULL
(defrest "/posts"
    'list-items
  'show-item
  'add-item
  'create-item
  'destroy-item)

(defrest "/posts"
    (:index 'list-items)
  (:show 'show-item)
  (:add 'add-item)
  (:create 'create-item)
  (:destroy 'destroy-item))

(defrest "/posts"
    (:index 'list-items
	    (:extra ";filter" 'list-filtered-items))
  (:show 'show-item
	 (:extra ";detail" 'show-detailed-item)
	 (:extra ";coded" 'show-barcoded-item))
  (:add 'add-item)
  (:create 'create-item)
  (:destroy 'destroy-item))


;; A complete system (cl-perec part is not correct)
(defclass item ()
  ((name :accessor name
	 :type :string)
   (description :accessor description
		:type :text)
   (code :accessor code
	 :type :integer)
   (id :reader id
       :type :id)
   (image :accessor image
	  :type 'image)))

(defun list-item (item)
  "Lists a single Item"
  (li :class "item"
      (span :class "name" (name item))))

(defun list-items ()
  (with-standard-page "List items"
    (h1 "list items")
    (ul :class "items"
	(loop for item in (get-all-items) collect
	     (list-item item)))))

(defun list-filtered-items ()
  "Lists the items in a filtered style.  The way the item is accessed is totally wrong right now, as is the way the given-variable is found."
  (with-standard-page "Filtered item list"
    (h1 "Filtered items")
    (div :class "filterArgs" (user-var :filter))
    (ul :class "items"
	(loop for item in (get-all-items :filter (user-var :filter)) collect
	     (list-item item)))))

(defun show-classed-slot (name value)
  (ul :class name
      (span :class "title" name)
      (span :class "content" value)))


(defun show-detailed-item (&optional item)
  "shows the detailed version of an item"
  (let ((item (or item 
		  (get-item :id (user-var :id)))))
    (list 
     (h1 "Detailed item")
     (div :class "detailed item"
	  (map 'list 
	       (lambda (fun) (show-classed-slot (string-downcase (print name)) (funcall fun item)))
	       '(name id description code))))))
  
(defun show-barcoded-item ()
  "shows the barcoded version of an item"
  (let ((item (get-item :id (user-var :id))))
    (list 
     (h1 "Barcoded item")
     (div :class "barcoded"
	  (show-classed-slot "name" (name item))
	  (show-classed-slot "barcode" (barcode item))))))

(defun add-item ()
  "Creates a new item.  Let's not do validations just yet."
  (apply initialize-instance 
	 'item 
	 (apply 'nconc (map 'list 
			    (lambda (var) (var (user-var var)))
			    '(:name :description :code)))))

(defun destroy-item ()
  "Destroys an item."
  (destroy-item :id (user-var :id)))
