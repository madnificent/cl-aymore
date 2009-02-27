(in-package :CLaymore.routing.tests)

(defsuite tests)
(in-suite tests)

(deftest has-as-url (page url &key (url-options nil))
  (util:debug-print page url url-options)
  (is (string= (apply 'CLaymore.routing:handler-url page url-options) url)))

(deftest has-as-page (url page)
  (is (eql (CLaymore.routing:page-handler url) page)))

(deftest interconnects (page route &key (url-options))
  (format T "--~%")
  (util:debug-print page route url-options)
  (is (and (has-as-url page route :url-options url-options)
	   (has-as-page route page))))

(deftest identified-variable (key var)
  (is (equal (CLaymore.routing:url-var key) var)))

(deftest stage1 ()
  (set-routing-table '(("about" about-overview
			("contact" contact)
			("overview" overview)
			("status" status))
		       ("welcome" welcome)))
  (has-as-page "/about/contact" 'contact)
  (has-as-page "/about" 'about-overview)
  (has-as-page "/welcome" 'welcome))

(deftest stage2 ()
  (set-routing-table '(("about" about-overview
			("contact" contact)
			("overview" overview)
			("status" status))
		       ("welcome" welcome)))
  (has-as-url 'contact "/about/contact")
  (has-as-url 'about-overview "/about")
  (has-as-url 'welcome "/welcome"))

(deftest stage2tot ()
  (set-routing-table '(index
		       ("about" about-overview
			("contact" contact)
			("overview" overview)
			("status" status))
		       ("welcome" welcome)))
  (interconnects 'index "/")
  (interconnects 'about-overview "/about")
  (interconnects 'contact "/about/contact")
  (interconnects 'overview "/about/overview")
  (interconnects 'status "/about/status")
  (interconnects 'welcome "/welcome"))

(deftest stage3-and-4 ()
  (set-routing-table '(("posts" posts
			("show"
			 (".*" (handler sets post-id)
			       show-post)))))
  (interconnects 'posts "/posts")
  (interconnects 'show-post "/posts/show/100" :url-options '(post-id "100"))
  (identified-variable 'post-id "100"))

(deftest stage-5 ()
  (set-routing-table '(("posts" (handles posts loosely))))
  (has-as-page "/posts" 'posts)
  (has-as-page "/posts/" 'posts))

(deftest stage-6 ()
  ;; I've chosen to use always and never to test.  This is to ensure that we don't need need to create special stuff for the tests.
  (set-routing-table '(("posts" (handles post-list loosely)
			("\\d+" (handler sets post-id)
			 (when never show-post)
			 (when always create-post)))
		       ("users" (handles users loosely)
			("\\d+" (handler sets user-id)
			 (handles show-user only-when always)
			 (handles update-user only-when never)))))
  (interconnects 'post-list "/posts")
  (interconnects 'create-post "/posts/123" :url-options '(post-id "123"))
  (interconnects 'users "/users")
  (interconnects 'show-user "/users/123" :url-options '(user-id "123")))
