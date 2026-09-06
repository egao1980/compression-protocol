(defsystem "compression-protocol"
  :version "0.1.1"
  :description "CLOS compression codecs + zip archive protocol for cl-stack"
  :author "egao1980"
  :license "MIT"
  :depends-on ("encoding-protocol")
  :properties (:cl-repo (:ci (:with ("compression-backend-chipz"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "protocol")
               (:file "zip"))
  :in-order-to ((test-op (test-op "compression-protocol/tests"))))

(defsystem "compression-protocol/tests"
  :depends-on ("compression-protocol" "compression-backend-chipz" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "codec-test")
               (:file "zip-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
