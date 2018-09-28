;;;; destructuring-bind-star.asd

(asdf:defsystem #:destructuring-bind-star
  :description "DESTRUCTURING-BIND with proper error signaling"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "destructuring-bind-star")))

(asdf:defsystem #:destructuring-bind-star/test
  :description "Test for DESTRUCTURING-BIND*"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "Public domain"
  :version "0.0.1"
  :serial t
  :depends-on (#:destructuring-bind-star)
  :components ((:file "test")))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system ':destructuring-bind-star))))
  (asdf:load-system :destructuring-bind-star/test)
  (uiop:symbol-call :destructuring-bind-star :test))
