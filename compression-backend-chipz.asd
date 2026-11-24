(defsystem "compression-backend-chipz"
  :version "0.3.0"
  :description "compression-protocol backend — chipz inflate + salza2 deflate (gzip/zlib/deflate) + bzip2 inflate"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "compression-protocol" "0.5.0") "chipz" "salza2"
               "trivial-gray-streams")
  :serial t
  :pathname "src/backend-chipz"
  :components ((:file "package")
               (:file "backend")))
