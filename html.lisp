;;;; This is the micro HTML-generation-library.  It is possible to make this more efficient by expanding the functions, but that can be implemented later.
;;;; TODO: Throw all the existing tags from the xhtml standard in here and implement them
(in-package :CLaymore.html.full)

(eval-when (:compile-toplevel :load-toplevel)
  (defun def-tag (name &optional (vname name))
    "Defines a tag by its name (and function-name).  The expansions will be automatically generated."
    (eval `(defun ,vname (&rest tag-data)
	     (multiple-value-bind (name attrs contents)
		 (split-name-attr-contents ',name tag-data)
	       (mktag name attrs contents))))
    (eval `(define-compiler-macro ,vname (&rest tag-data)
	     (multiple-value-bind (name attrs contents)	
		 (split-name-attr-contents ',name tag-data)
	       (list 'mktag name attrs contents))))))
  
;; The following makes it easy to define new tags, yet it does not yet make it efficient.
;; It does, however, give us a place in which we may macro-expand or compiler-macro-expand to precomputed everything we know already
(eval-when (:compile-toplevel :load-toplevel)
  ;; tags with regular names
  (dolist (tag '(a abbr acronym address area b base bdo big blockquote body br button caption cite code col colgroup dd del div dfn dl dt em fieldset form h1 h2 h3 h4 h5 h6 head hr html i img input ins kbd label legend li link meta noscript object ol optgroup option p param pre q samp script select small span strong style sub sup table tbody td textarea tfoot th thead title tr tt ul))
    (def-tag tag))
  ;; tags with modified names (due to name-clashes)
  (dolist (tag-def '((map image-map) (var xhtml-var)))
    (def-tag (first tag-def) (second tag-def))))

;;;;;;;;;;;;;;
;; dirty cases
(defun !-- (comments &rest body)
  "(x)html comment"
  (format nil "<!--~A-->~{~A~}"
	  (apply #'strcon comments)
	  body))
(defun !doctype (string &rest body)
  "Please expect this to change in the future, as it should accept a keyword for the most common doctypes"
  (format nil "<!DOCTYPE ~A>~{~A~}" string body))

;;;;;;;;;;;;;;;;;;
;; the actual code
(defun split-name-attr-contents (tag tag-content)
  "Splits the definition of a tag in it's name (which is stringified), its attribute-value-pairs and its content."
  (let ((tag (string-downcase (string tag)))
	(content tag-content)
	(key-vals nil))
    (loop named eat-attributes while T do
	 (if (keywordp (first content))
	     (progn (setf key-vals (concatenate 'list key-vals `((,(interpret-tag-name (string (first content))) ,(second content)))))
		    (setf content (cddr content)))
	     (return-from eat-attributes)))
    (values tag key-vals content)))
    
(defgeneric htmlify (object)
  (:documentation "Creates a string from the given object, in order for it to be printable"))
(defmethod htmlify (object)
  (format nil "~A" object))
(defmethod htmlify ((s string))
  s)

(defun strcon (&rest args)
  "Concatenates strings together"
  (apply 'concatenate 
	 'string
	 (loop for x in args collect
	      (if (listp x)
		  (apply 'strcon x)
		  (htmlify x)))))

(define-compiler-macro strcon (&whole form &rest args &environment env)
  ;; the real expander-function is str-con-on-own-level.  The other code is there to ensure the inner macroexpansions are done before running on this level.
  ;; even though not perfect, it should suffice for this purpose
  (flet ((str-con-on-own-level (form args)
	   (labels ((concat-subsequent-strings (list)
		      (if (and (stringp (first list))
			       (stringp (second list)))
			  (concat-subsequent-strings `(,(concatenate 'string (first list) (second list)) ,@(cddr list)))
			  (if (> (length list) 1)
			      `(,(first list) ,@(concat-subsequent-strings (rest list)))
			      list))))
	     (let ((result (concat-subsequent-strings (apply 'concatenate 'list 
							     (map 'list (lambda (arg) (if (and (listp arg) (eql (first arg) 'strcon))
											  (rest arg)
											  (list arg)))
								  args)))))
	       (cond ((= 1 (length result))
		      (first result))
		     ((equal `(strcon ,@result) form)
		      form)
		     (T
		      `(strcon ,@result))))))
	 (expand-arg (arg env)
	   (if (and arg (listp arg) (compiler-macro-function (first arg) env))
	       (funcall (compiler-macro-function (first arg) env) arg nil)
	       arg)))
    (str-con-on-own-level form (map 'list (lambda (x) (expand-arg x env)) args))))
      
(defun mktag (name attrs &optional constr)
  "Creates a tag, in which <name> is the name of the tag (eg: \"div\"), <attrs> contains the attributes and <constr> contains the strings that are contained in it.
Only constr is allowed to contain a list of strings (or functions that will generate strings) in order to obtain a correct result."
  (if constr
      (strcon (mk-start-tag name attrs) (apply 'strcon constr) (mk-end-tag name))
      (mk-empty-tag name attrs)))
(define-compiler-macro mktag (&whole form name attrs &optional constr)
  (if (listp attrs) ; this should tell us that this is an expansion in which we know the arguments (constr wouldn't be a meaningless symbol in this case)
      (if constr 
	  `(strcon (mk-start-tag ,name ,attrs)
		   ,(if (listp constr) 
			`(strcon ,@constr)
			`(apply 'strcon ,constr))
		   (mk-end-tag ,name))
	  `(mk-empty-tag ,name ,attrs))
      form))

(defun mk-end-tag (name)
  "Creates an end-tag for <name>.
This means that <name> will be converted to <<name>/>"
  (strcon "</" name ">"))
(define-compiler-macro mk-end-tag (&whole form name)
  (declare (ignore form))
  `(strcon "</" ,name ">"))

(defun mk-empty-tag (name attrs)
  "Creates an empty tag for <name> This means the tag doesn't close over any data.
example: (mk-empty-tag \"foo\" (list :bar \"baz\")) will expand to <foo bar=\"baz\"/>"
  (strcon "<" name (mk-attr-list attrs) " />"))
(define-compiler-macro mk-empty-tag (&whole form name attrs)
  (declare (ignore form))
  `(strcon "<" ,name (mk-attr-list ,attrs) " />"))
  
(defun mk-start-tag (name attrs)
  "Creates a start tag for <name> and <attrs>.
example: (mk-start-tag \"foo\" (list :bar \"baz\")) will expand to <foo bar=\"baz\">"
  (strcon "<" name (mk-attr-list attrs) ">"))
(define-compiler-macro mk-start-tag (&whole form name attrs)
  (declare (ignore form))
  `(strcon "<" ,name (mk-attr-list ,attrs) ">"))

(defun mk-attr-list (attrs)
  "Creates an attribute-list from attrs"
  (if attrs
      (format nil "~{ ~{~A=\"~A\"~}~}" attrs)
      ""))
(define-compiler-macro mk-attr-list (&whole form attrs)
  (if (listp attrs)
      (if attrs
	  `(strcon ,@(loop for attr-comb in attrs collect
			  `(strcon " " ,(upcase-after-dash (first attr-comb)) "=\"" ,(second attr-comb) "\"")))
	  "")
      form))

(defun interpret-tag-name (tag)
  (upcase-after-dash (string-downcase (format nil "~A" tag))))

(defun upcase-word (word)
	   (apply #'concatenate 'string
		  (list (char-upcase (elt word 0)))
		  (loop for char in (rest (concatenate 'list word)) collect (list char))))

(defun upcase-after-dash (word)
  (let ((split (cl-ppcre:split "-" word)))
    (apply #'concatenate 'string (first split)
	   (loop for part in (rest split)
	      collect (upcase-word part)))))
