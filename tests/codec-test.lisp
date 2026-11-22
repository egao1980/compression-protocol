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

;;; A backend that allocates up front sees the limit before reading input.
(defvar *seen-max-output* :unset)

(defmethod make-decompressing-stream-using-algorithm
    ((algorithm (eql :test-declared-length)) input &key max-output)
  (setf *seen-max-output* max-output)
  (when (and max-output (> 4294967296 max-output))
    (error 'compression-limit-exceeded :algorithm algorithm :limit max-output))
  input)

(deftest max-output-reaches-backend
  (let ((*seen-max-output* :unset))
    (ok (signals (decompress (make-array 5 :element-type '(unsigned-byte 8))
                             :algorithm :test-declared-length :max-output 1024)
                 'compression-limit-exceeded))
    (ok (eql 1024 *seen-max-output*)))
  (let ((*seen-max-output* :unset))
    (make-decompressing-stream (compression-protocol::%octet-input #())
                               :algorithm :test-declared-length)
    (ok (null *seen-max-output*)))
  ;; Backends whose methods take no keys still accept :max-output.
  (let ((bomb (compress (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)
                        :algorithm :gzip)))
    (ok (= 100 (length (decompress bomb :algorithm :gzip :max-output 100))))
    (ok (streamp (make-decompressing-stream (compression-protocol::%octet-input bomb)
                                            :algorithm :gzip :max-output 100)))))
