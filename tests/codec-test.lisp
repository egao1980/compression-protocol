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

(deftest bounded-decompress
  (let* ((zeros (make-array 1000000 :element-type '(unsigned-byte 8) :initial-element 0))
         (bomb (compress zeros :algorithm :zlib)))
    (ok (< (* 50 (length bomb)) (length zeros)))   ; a bomb: far smaller than its output
    (ok (equalp (decompress bomb :algorithm :zlib :max-output 1000000) zeros))
    (ok (signals (decompress bomb :algorithm :zlib :max-output 999999)
                 'compression-limit-exceeded))
    (ok (= 1000 (handler-case (decompress bomb :algorithm :zlib :max-output 1000)
                  (compression-limit-exceeded (c) (compression-limit-exceeded-limit c)))))
    ;; A stream source works the same way.
    (with-open-file (out "/tmp/cp-bomb.z" :direction :output :element-type '(unsigned-byte 8)
                                          :if-exists :supersede)
      (write-sequence bomb out))
    (with-open-file (in "/tmp/cp-bomb.z" :element-type '(unsigned-byte 8))
      (ok (= 1000000 (length (decompress in :algorithm :zlib :max-output 2000000)))))))
