;;;; secretary.asd

(asdf:defsystem #:secretary
  :description "Events and facts"
  :author "Will Langstroth <will@langstroth.com>"
  :license "MIT"
  :serial t
  :depends-on (#:st-json #:drakma)
  :components ((:file "package")
               (:file "comparators")
               (:file "time")
               (:file "oanda")
               (:file "secretary")))
