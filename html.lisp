;;;; This is the micro HTML-generation-library.  It is possible to make this more efficient by expanding the functions, but that can be implemented later.
;;;; TODO: Throw all the existing tags from the xhtml standard in here and implement them
(in-package :CLaymore.html)

;; The following makes it easy to define new tags, yet it does not yet make it efficient.
;; It does, however, give us a place in which we may macro-expand or compiler-macro-expand to precomputed everything we know already
(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (tag '(html head title body h1 h2 h3 h4 h5 h6 table tr th td p div ul ol li span strong a br hr form input textarea style b i em hidden label img script link))
    (eval `(defun ,tag (&rest tag-data)
	     (multiple-value-bind (name attrs contents)
		 (split-name-attr-contents ',tag tag-data)
	       (mktag name attrs contents))))))

;;;;;;;;;;;;;;;;;;
;; the actual code
(defun split-name-attr-contents (tag tag-content)
  "Splits the definition of a tag in it's name (which is stringified), its attribute-value-pairs and its content."
  (let ((tag (string-downcase (string tag)))
	(content tag-content)
	(key-vals nil))
    (loop named eat-attributes while T do
	 (if (keywordp (first content))
	     (progn (setf key-vals (concatenate 'list key-vals `((,(string-downcase (string (first content))) ,(second content)))))
		    (setf content (cddr content)))
	     (return-from eat-attributes)))
    (values tag key-vals content)))
    
(define-compiler-macro strcon (&rest args)
  (let ((func (compiler-macro-function 'strcon)))
    (cond ((eql (first args) 'strcon)
	   (apply func (rest (first args)) (rest args)))
	  ((and (stringp (first args)) (stringp (first (apply func (rest args)))))
	   `(strcon ,(concatenate 'string (first args) (first (apply func (rest args)))) ,@(rest (apply func (rest args)))))
	  (T
	   `(strcon ,@args)))))

(defun strcon (&rest args)
  "Concatenates strings together"
  (apply 'concatenate 
	 'string
	 (loop for x in args collect
	      (if (listp x)
		  (apply 'strcon x)
		  x))))

(defun mktag (name attrs &optional constr)
  "Creates a tag, in which <name> is the name of the tag (eg: \"div\"), <attrs> contains the attributes and <constr> contains the strings that are contained in it.
Only constr is allowed to contain a list of strings (or functions that will generate strings) in order to obtain a correct result."
  (if constr
      (strcon (mk-start-tag name attrs) (apply 'strcon constr) (mk-end-tag name))
      (mk-empty-tag name attrs)))

(defun mk-end-tag (name)
  "Creates an end-tag for <name>.
This means that <name> will be converted to <<name>/>"
  (strcon "<" name "/>"))

(defun mk-empty-tag (name attrs)
  "Creates an empty tag for <name> This means the tag doesn't close over any data.
example: (mk-empty-tag \"foo\" (list :bar \"baz\")) will expand to <foo bar=\"baz\"/>"
  (strcon "<" name (mk-attr-list attrs) " />"))
  
(defun mk-start-tag (name attrs)
  "Creates a start tag for <name> and <attrs>.
example: (mk-start-tag \"foo\" (list :bar \"baz\")) will expand to <foo bar=\"baz\">"
  (strcon "<" name (mk-attr-list attrs) ">"))

(defun mk-attr-list (attrs)
  "Creates an attribute-list from attrs"
  (if attrs
      (format nil "~{ ~{~A=\"~A\"~}~}" attrs)
      ""))
