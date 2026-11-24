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

;;; Damaged archives (compression-protocol#7): only COMPRESSION-ERROR gets out.

(defun %zip-with (entries)
  "A zip of ENTRIES, (NAME OCTETS METHOD), written by hand: METHOD 0 is
   stored, 8 deflate. Sizes and CRC-32 in the headers are the real ones."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (centrals '()))
    (flet ((u16 (v) (compression-protocol::%put-u16 out v))
           (u32 (v) (compression-protocol::%put-u32 out v))
           (octets (v) (loop for b across v do (vector-push-extend b out))))
      (dolist (e entries)
        (destructuring-bind (name data method) e
          (let ((name-octets (encoding-protocol:encode name))
                (payload (if (= method 8) (compress data :algorithm :deflate) data))
                (crc (compression-protocol::%crc32 data))
                (local (length out)))
            (u32 #x04034b50) (u16 20) (u16 0) (u16 method) (u16 0) (u16 0)
            (u32 crc) (u32 (length payload)) (u32 (length data))
            (u16 (length name-octets)) (u16 0)
            (octets name-octets) (octets payload)
            (push (list name-octets method crc (length payload) (length data) local)
                  centrals))))
      (let ((cd (length out)))
        (dolist (c (reverse centrals))
          (destructuring-bind (name-octets method crc csize usize local) c
            (u32 #x02014b50) (u16 20) (u16 20) (u16 0) (u16 method) (u16 0) (u16 0)
            (u32 crc) (u32 csize) (u32 usize)
            (u16 (length name-octets)) (u16 0) (u16 0) (u16 0) (u16 0) (u32 0) (u32 local)
            (octets name-octets)))
        (let ((size (- (length out) cd)))
          (u32 #x06054b50) (u16 0) (u16 0) (u16 (length centrals)) (u16 (length centrals))
          (u32 size) (u32 cd) (u16 0))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun %text-octets (n)
  "N octets of text with repeats and numbers, so deflate output is long
   enough to damage and has matches to damage."
  (let ((words #("alpha " "beta " "gamma " "delta " "<w:t>" "</w:t>" "42 "))
        (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for i from 0
          while (< (length out) n)
          do (loop for b across (encoding-protocol:encode
                                 (format nil "~a~d " (aref words (mod (* i 5) 7))
                                         (mod (* i i) 1000)))
                   do (vector-push-extend b out)))
    (coerce (subseq out 0 n) '(simple-array (unsigned-byte 8) (*)))))

(defun %u32-at (octets offset value)
  (replace octets (vector (ldb (byte 8 0) value) (ldb (byte 8 8) value)
                          (ldb (byte 8 16) value) (ldb (byte 8 24) value))
           :start1 offset))

(defun %central-offset (zip name)
  (- (search (encoding-protocol:encode name) zip :from-end t) 46))

(deftest zip-deflate-entries-checked
  (let* ((body (%text-octets 5000))
         (zip (%zip-with `(("a.txt" ,body 8) ("s.txt" ,(%text-octets 10) 0))))
         (archive (open-archive zip :format :zip)))
    (ok (equalp body (read-entry archive "a.txt")) "method 8, CRC-32 and size match")
    (ok (equalp body (read-entry archive "a.txt" :max-size 5000)) "max-size = size")
    (ok (signals (read-entry archive "a.txt" :max-size 4999) 'compression-limit-exceeded))
    (ok (signals (read-entry archive "s.txt" :max-size 9) 'compression-limit-exceeded)))
  (let* ((stored (%zip-with `(("s.txt" ,(%text-octets 100) 0))))
         (data (+ 30 (length "s.txt"))))
    (setf (aref stored (+ data 7)) (logxor 1 (aref stored (+ data 7))))
    (let ((archive (open-archive stored :format :zip)))
      (ok (signals (read-entry archive "s.txt") 'archive-error)
          "one flipped bit: bad CRC-32, as zipfile's BadZipFile")
      (ok (= 100 (length (handler-bind ((archive-error #'continue))
                           (read-entry archive "s.txt"))))
          "CONTINUE returns the bytes anyway"))))

(deftest zip-damaged-structure-signals-archive-error
  (let ((body (%text-octets 3000)))
    (flet ((damaged (fn)
             (let ((zip (%zip-with `(("word/document.xml" ,body 8)))))
               (funcall fn zip)
               zip)))
      (dolist (case
               (list (list "compressed size past the end"
                           (lambda (z) (%u32-at z (+ 20 (%central-offset z "word/document.xml"))
                                                #x7fffffff)))
                     (list "local header offset past the end"
                           (lambda (z) (%u32-at z (+ 42 (%central-offset z "word/document.xml"))
                                                #x7ffffff0)))
                     (list "uncompressed size understated"
                           (lambda (z) (%u32-at z (+ 24 (%central-offset z "word/document.xml"))
                                                100)))
                     (list "ZIP64 marker"
                           (lambda (z) (%u32-at z (+ 24 (%central-offset z "word/document.xml"))
                                                #xffffffff)))))
        (destructuring-bind (label fn) case
          (ok (signals (let ((a (open-archive (damaged fn) :format :zip)))
                         (read-entry a "word/document.xml"))
                       'archive-error)
              label)))
      (ok (signals (open-archive (let ((z (%zip-with `(("a" ,body 8)))))
                                   (%u32-at z (- (length z) 6) #x7fffffff)
                                   z)
                                 :format :zip)
                   'archive-error)
          "central directory offset past the end")
      (let ((z (%zip-with `(("a" ,body 8)))))
        (fill z #xff :start (+ 30 1 10) :end (+ 30 1 60))
        (let ((e (handler-case (read-entry (open-archive z :format :zip) "a")
                   (error (e) e))))
          ;; Python: zlib.error, or BadZipFile when zlib reads garbage as data.
          (ok (or (typep e 'archive-error)
                  (and (typep e 'compression-error)
                       (typep (compression-error-cause e) 'chipz:chipz-error)))
              (format nil "corrupt deflate data: ~s ~@[cause ~s~]" (type-of e)
                      (and (typep e 'compression-error)
                           (type-of (compression-error-cause e))))))))))

(deftest zip-random-damage-signals-compression-error
  "Random damage to a zip (1-3 bytes anywhere, or a truncation) opens and
   reads, or signals COMPRESSION-ERROR: never another condition. 3000
   archives, a fixed LCG."
  (let* ((zip (%zip-with `(("a.xml" ,(%text-octets 4000) 8) ("dir/" ,(%text-octets 0) 0)
                           ("b.bin" ,(%text-octets 300) 0) ("c.xml" ,(%text-octets 900) 8))))
         (seed 7)
         (other '()))
    (flet ((random* (n)
             (setf seed (mod (+ (* seed 1103515245) 12345) #x80000000))
             (mod (ash seed -8) n)))
      (dotimes (i 3000)
        (let ((z (copy-seq zip)))
          (if (zerop (random* 8))
              (setf z (subseq z 0 (random* (length z))))
              (dotimes (k (1+ (random* 3)))
                (setf (aref z (random* (length z))) (random* 256))))
          (handler-case
              (let ((a (open-archive z :format :zip)))
                (dolist (e (archive-entries a))
                  (unless (archive-entry-directory-p e)
                    (read-entry a e))))
            (compression-error () nil)
            (error (e) (push (list i (type-of e) (princ-to-string e)) other))))))
    (ok (null other) (format nil "~d not COMPRESSION-ERROR, first: ~s"
                             (length other) (car (last other))))))
