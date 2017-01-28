;;;; secretary.asd

(asdf:defsystem #:secretary
  :description "Events and facts"
  :author "Will Langstroth <will@langstroth.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "secretary")))
