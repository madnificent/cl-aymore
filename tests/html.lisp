(in-package :CLaymore.html.tests)

(defsuite tests)
(in-suite tests)

;; this is tested by example
(deftest nesting ()
  (is (string= "<html><head><title>foobar</title></head></html>" 
	       (html (head (title "foobar"))))))

(deftest basic-enumeration ()
  (is (string= "<html><head><title>foobar</title></head><body><p>for teh win</p></body></html>" 
	       (html (head (title "foo" "bar")) (body (p "for teh win"))))))

(deftest sublist-enumeration ()
  (is (string= "<html><head><title>foobar</title></head><body><p>for teh <em>win</em></p></body></html>"
	       (html (head (title "foo" '("bar"))) (body (p "for" (list " " "teh " (em "win"))))))))

(deftest options ()
  (is (string= "<html lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\"><head><title>win</title></head></html>"
	       (html :lang "en" :xmlns "http://www.w3.org/1999/xhtml" :|xml:lang| "en" (head (title "win"))))))

