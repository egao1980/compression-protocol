(in-package #:compression-protocol)

;;; ustar (POSIX.1-1988) read/write. Names ≤100 bytes, or prefix+name.

(defclass tar-archive (archive)
  ((bytes :initarg :bytes :reader tar-archive-bytes)
   (entries :initform nil :accessor tar-archive-entry-list)))

(defun %ascii-field (bytes start len)
  (let* ((raw (map 'string #'code-char (subseq bytes start (+ start len))))
         (nul (position #\Nul raw)))
    (string-right-trim '(#\Space) (if nul (subseq raw 0 nul) raw))))

(defun %octal-field (bytes start len)
  (let* ((s (string-trim '(#\Space #\Nul) (%ascii-field bytes start len))))
    (if (zerop (length s))
        0
        (parse-integer s :radix 8))))

(defun %zero-block-p (bytes start)
  (loop for i from start below (+ start 512)
        always (zerop (aref bytes i))))

(defun %tar-checksum (header)
  (let ((sum 0))
    (dotimes (i 512 sum)
      (incf sum (if (<= 148 i 155) 32 (aref header i))))))

(defun %put-ascii (vec start width string)
  (let* ((octets (encoding-protocol:encode (or string "")))
         (n (min (length octets) (1- width))))
    (loop for i below n do (setf (aref vec (+ start i)) (aref octets i)))))

(defun %put-octal (vec start width value)
  (let ((s (format nil "~v,'0o" (1- width) value)))
    (when (>= (length s) width)
      (error 'archive-error :message (format nil "octal field overflow: ~a" value)))
    (loop for i from 0 below (1- width)
          do (setf (aref vec (+ start i)) (char-code (char s i))))
    (setf (aref vec (+ start (1- width))) 0)))

(defun %tar-join-name (prefix name)
  (cond
    ((zerop (length prefix)) name)
    ((zerop (length name)) prefix)
    (t (concatenate 'string prefix "/" name))))

(defun %tar-split-name (name)
  (let ((name (string-left-trim "/" name)))
    (if (<= (length name) 100)
        (values name "")
        (let ((slash (position #\/ name :end (min (length name) 156) :from-end t)))
          (if (and slash (< (- (length name) slash 1) 100) (<= slash 155))
              (values (subseq name (1+ slash)) (subseq name 0 slash))
              (error 'archive-error
                     :message (format nil "tar name too long: ~a" name)))))))

(defun %open-tar-archive (source)
  (let* ((bytes (if (and (vectorp source) (not (stringp source)))
                    source
                    (%source-bytes source)))
         (archive (make-instance 'tar-archive :format :tar :bytes bytes))
         (entries '())
         (off 0)
         (len (length bytes)))
    (loop
      (when (>= (+ off 512) len)
        (return))
      (when (%zero-block-p bytes off)
        (return))
      (let* ((header (subseq bytes off (+ off 512)))
             (sum (%tar-checksum header))
             (stored (%octal-field header 148 8)))
        (unless (= sum stored)
          (error 'archive-error
                 :message (format nil "corrupt tar checksum at ~d" off)))
        (let* ((name (%ascii-field header 0 100))
               (prefix (%ascii-field header 345 155))
               (full (%tar-join-name prefix name))
               (size (%octal-field header 124 12))
               (mtime (%octal-field header 136 12))
               (typeflag (code-char (aref header 156)))
               (dir-p (or (char= typeflag #\5)
                          (and (plusp (length full))
                               (char= (char full (1- (length full))) #\/))))
               (data-off (+ off 512))
               (padded (* 512 (ceiling size 512))))
          (push (make-instance 'archive-entry
                               :name full
                               :directory-p dir-p
                               :method 0
                               :uncompressed-size size
                               :compressed-size size
                               :header-offset data-off
                               :dos-date mtime)
                entries)
          (incf off (+ 512 padded)))))
    (setf (tar-archive-entry-list archive) (nreverse entries))
    archive))

(defmethod archive-entries ((archive tar-archive))
  (copy-list (tar-archive-entry-list archive)))

(defun %find-tar-entry (archive entry)
  (if (archive-entry-p entry)
      entry
      (or (find entry (tar-archive-entry-list archive)
                :key #'archive-entry-name :test #'string=)
          (error 'archive-error :message (format nil "tar entry not found: ~a" entry)))))

(defmethod read-entry ((archive tar-archive) entry)
  (let ((ent (%find-tar-entry archive entry)))
    (when (archive-entry-directory-p ent)
      (error 'archive-error
             :message (format nil "not a file entry: ~a" (archive-entry-name ent))))
    (let ((off (archive-entry-header-offset ent))
          (size (archive-entry-uncompressed-size ent)))
      (subseq (tar-archive-bytes archive) off (+ off size)))))

(defun %write-tar-header (out name size typeflag)
  (multiple-value-bind (base prefix) (%tar-split-name name)
    (let ((header (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0)))
      (%put-ascii header 0 100 base)
      (%put-octal header 100 8 (if (char= typeflag #\5) #o755 #o644))
      (%put-octal header 108 8 0)
      (%put-octal header 116 8 0)
      (%put-octal header 124 12 size)
      (%put-octal header 136 12 (- (get-universal-time) 2208988800))
      (loop for i from 148 below 156 do (setf (aref header i) 32))
      (setf (aref header 156) (char-code typeflag))
      (%put-ascii header 257 6 "ustar")
      (setf (aref header 263) (char-code #\0)
            (aref header 264) (char-code #\0))
      (%put-ascii header 345 155 prefix)
      (let ((sum (%tar-checksum header))
            (digits (format nil "~6,'0o" sum)))
        (loop for i from 0 below 6
              do (setf (aref header (+ 148 i)) (char-code (char digits i))))
        (setf (aref header 154) 0
              (aref header 155) 32))
      (loop for b across header do (vector-push-extend b out)))))

(defun %write-tar-bytes (entries)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (pair entries)
      (destructuring-bind (name data) pair
        (let* ((name (string-left-trim "/" (string name)))
               (dir-p (or (eq data :directory)
                          (and (plusp (length name))
                               (char= (char name (1- (length name))) #\/))))
               (payload (cond
                          (dir-p #())
                          ((stringp data) (encoding-protocol:encode data))
                          (t (coerce data '(vector (unsigned-byte 8))))))
               (name (if (and dir-p (or (zerop (length name))
                                        (char/= (char name (1- (length name))) #\/)))
                         (concatenate 'string name "/")
                         name)))
          (%write-tar-header out name (length payload) (if dir-p #\5 #\0))
          (loop for b across payload do (vector-push-extend b out))
          (let ((pad (mod (- 512 (mod (length payload) 512)) 512)))
            (dotimes (_ pad) (vector-push-extend 0 out))))))
    (dotimes (_ 1024) (vector-push-extend 0 out))
    out))

(defun %canonical-archive-format (format)
  (ecase format
    ((:zip) :zip)
    ((:tar :ustar) :tar)
    ((:tar.gz :tgz) :tar.gz)
    ((:tar.bz2 :tbz2 :tbz) :tar.bz2)))

(defmethod open-archive (source &key (format :zip))
  (let ((fmt (%canonical-archive-format format)))
    (ecase fmt
      (:zip (%open-zip-archive source))
      (:tar (%open-tar-archive source))
      (:tar.gz
       (%open-tar-archive (decompress (%source-bytes source) :algorithm :gzip)))
      (:tar.bz2
       (%open-tar-archive (decompress (%source-bytes source) :algorithm :bzip2))))))

(defun write-archive-bytes (entries &key (format :zip))
  "Build an archive as a byte vector. FORMAT :zip (stored), :tar, or :tar.gz.
   ENTRIES is a list of (name string-or-octets). :tar.bz2 write is unsupported."
  (let ((fmt (%canonical-archive-format format)))
    (ecase fmt
      (:zip (%write-zip-bytes entries))
      (:tar (%write-tar-bytes entries))
      (:tar.gz (compress (%write-tar-bytes entries) :algorithm :gzip))
      (:tar.bz2
       (error 'unsupported-algorithm
              :algorithm format
              :message "write-archive-bytes: no bzip2 compressor")))))
