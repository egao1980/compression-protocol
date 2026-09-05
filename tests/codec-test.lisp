(in-package #:compression-protocol/tests)

(deftest gzip-roundtrip
  (let* ((raw (encoding-protocol:encode "hello compression"))
         (gz (compress raw :algorithm :gzip))
         (back (decompress gz :algorithm :gzip)))
    (ok (equalp raw back))))

(deftest zlib-roundtrip
  (let* ((raw (encoding-protocol:encode "zlib payload"))
         (z (compress raw :algorithm :zlib)))
    (ok (equalp raw (decompress z :algorithm :zlib)))))

(deftest deflate-roundtrip
  (let* ((raw (encoding-protocol:encode "raw deflate"))
         (d (compress raw :algorithm :deflate)))
    (ok (equalp raw (decompress d :algorithm :deflate)))))

(deftest empty-gzip
  (let ((gz (compress #() :algorithm :gzip)))
    (ok (zerop (length (decompress gz :algorithm :gzip))))))

(deftest unsupported-br
  (ok (signals (compress #(1 2 3) :algorithm :br) 'unsupported-algorithm)))

(deftest string-input
  (ok (string= "hi"
               (encoding-protocol:decode
                (decompress (compress "hi" :algorithm :gzip) :algorithm :gzip)))))
