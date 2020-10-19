;;;; ciao.asd

(asdf:defsystem #:ciao
  :description "OAuth 2.0 Client for Common Lisp"
  :author "Jin-Ho King <jinho_king@tasseo.io>"
  :license  "LGPL"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot
               #:dexador
               #:jsown
               #:cl-ppcre
               #:trivial-open-browser)
  :components ((:file "package")
               (:file "ciao")))
