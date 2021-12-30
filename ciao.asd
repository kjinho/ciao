
(defsystem "ciao"
  :description "OAuth 2.0 Client for Common Lisp"
  :author "Jin-Ho King <jinho_king@tasseo.io>"
  :license  "LGPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot
               #:dexador
               #:cl-json
               #:cl-ppcre
               #:trivial-open-browser)
  :components ((:module "src"
                :components 
                ((:file "ciao"))))
  :in-order-to ((test-op (test-op "ciao/tests"))))

(defsystem "ciao/tests"
  :author "Jin-Ho King"
  :license "LGPLv3"
  :depends-on ("ciao"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for ciao"
  :perform (test-op (op c) (symbol-call :rove :run c)))
