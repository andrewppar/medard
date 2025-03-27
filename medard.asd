(defsystem "medard"
    :author "Andrew Parisi <andrew.p.parisi@gmail.com>"
    :license "Public Domain"
    :version "0.0.5"
    :depends-on ("cl-json" "dexador" "local-time")
    :components ((:module "src"
			  :components
			  ((:file "package")
			   (:file "main" :depends-on ("brightsky" "utils"))
			   (:file "brightsky")
			   (:file "utils"))))
    ;; :in-order-to ((test-op (test-op "tfcserver-test")))
    ;; :defsystem-depends-on (:deploy)  ;; (ql:quickload "deploy") before
    :build-operation "program-op" ;; leave as is
    :build-pathname "medard"
    :entry-point "medard::main")
