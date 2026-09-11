(defsystem "compression-backend-chipz"
  :version "0.1.2"
  :description "compression-protocol backend — chipz inflate + salza2 deflate (gzip/zlib/deflate) + bzip2 inflate"
  :author "egao1980"
  :license "MIT"
  :depends-on ("compression-protocol" "chipz" "salza2")
  :serial t
  :pathname "src/backend-chipz"
  :components ((:file "package")
               (:file "backend")))
