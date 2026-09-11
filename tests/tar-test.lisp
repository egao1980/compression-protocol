(in-package #:compression-protocol/tests)

(deftest tar-stored-roundtrip
  (let* ((bytes (write-archive-bytes
                 '(("countries/DE.sexp" "(:code \"DE\")")
                   ("empty.txt" ""))
                 :format :tar))
         (archive (open-archive bytes :format :tar))
         (names (mapcar #'archive-entry-name (archive-entries archive))))
    (ok (member "countries/DE.sexp" names :test #'string=))
    (ok (member "empty.txt" names :test #'string=))
    (ok (string= "(:code \"DE\")"
                 (encoding-protocol:decode (read-entry archive "countries/DE.sexp"))))
    (ok (zerop (length (read-entry archive "empty.txt"))))
    (close-archive archive)))

(deftest tar-gz-roundtrip
  (let* ((bytes (write-archive-bytes '(("a.txt" "hello tar")) :format :tar.gz))
         (archive (open-archive bytes :format :tgz)))
    (ok (string= "hello tar"
                 (encoding-protocol:decode (read-entry archive "a.txt"))))
    (close-archive archive)))

(defparameter *hello-bz2*
  (coerce #(66 90 104 57 49 65 89 38 83 89 193 192 128 226 0 0 1 65 0 0 16 2
            68 160 0 48 205 0 195 70 41 151 23 114 69 56 80 144 193 192 128 226)
          '(simple-array (unsigned-byte 8) (*))))

(defparameter *hello-lf*
  (coerce #(104 101 108 108 111 10) '(simple-array (unsigned-byte 8) (*))))

(deftest bzip2-decompress
  ;; Octets, not a source-file newline — Windows checkout is CRLF.
  (ok (equalp *hello-lf* (decompress *hello-bz2* :algorithm :bzip2)))
  (ok (equalp *hello-lf* (decompress *hello-bz2* :algorithm :bz2)))
  (ok (signals (compress "x" :algorithm :bzip2) 'unsupported-algorithm)))

(deftest tar-bz2-open
  (let* ((tar (write-archive-bytes `(("hi.txt" ,*hello-lf*)) :format :tar))
         (archive (open-archive tar :format :tar)))
    (ok (equalp *hello-lf* (read-entry archive "hi.txt")))
    (ok (signals (write-archive-bytes '(("a" "b")) :format :tar.bz2)
                 'unsupported-algorithm))))

(deftest xz-still-unsupported
  (ok (signals (decompress #(1 2 3) :algorithm :xz) 'unsupported-algorithm)))
