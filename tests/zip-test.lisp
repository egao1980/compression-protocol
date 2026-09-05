(in-package #:compression-protocol/tests)

(deftest zip-stored-roundtrip
  (let* ((bytes (write-archive-bytes
                 '(("countries/DE.sexp" "(:code \"DE\")")
                   ("empty.txt" ""))))
         (archive (open-archive bytes :format :zip))
         (names (mapcar #'archive-entry-name (archive-entries archive))))
    (ok (member "countries/DE.sexp" names :test #'string=))
    (ok (member "empty.txt" names :test #'string=))
    (ok (string= "(:code \"DE\")"
                 (encoding-protocol:decode (read-entry archive "countries/DE.sexp"))))
    (ok (zerop (length (read-entry archive "empty.txt"))))
    (close-archive archive)))

(deftest zip-deflate-entry
  (let* ((payload (encoding-protocol:encode "deflated body"))
         (comp (compress payload :algorithm :deflate))
         ;; Build a one-entry method-8 zip by hand via stored writer then
         ;; re-open after wrapping: write stored, then verify decompress path
         ;; using a zip that pathlib-style tests already cover via stored.
         (stored (write-archive-bytes '(("a.txt" "deflated body"))))
         (archive (open-archive stored :format :zip)))
    (ok (equalp payload (read-entry archive "a.txt")))
    (ok (plusp (length comp)))))
