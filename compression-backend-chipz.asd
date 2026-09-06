(defsystem "compression-backend-chipz"
  :version "0.1.1"
  :description "compression-protocol backend — chipz inflate + salza2 deflate (gzip/zlib/deflate)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("compression-protocol" "chipz" "salza2")
  :serial t
  :pathname "src/backend-chipz"
  :components ((:file "package")
               (:file "backend")))
