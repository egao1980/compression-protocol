(in-package #:compression-protocol)

;;; ZIP (APPNOTE): method 0 stored, method 8 deflate via COMPRESS/DECOMPRESS.

(defparameter +zip-local-sig+ #x04034b50)
(defparameter +zip-central-sig+ #x02014b50)
(defparameter +zip-eocd-sig+ #x06054b50)

(defclass zip-archive (archive)
  ((bytes :initarg :bytes :reader zip-archive-bytes)
   (entries :initform nil :accessor zip-archive-entry-list)))

(defun %u16 (bytes offset)
  (logior (aref bytes offset)
          (ash (aref bytes (1+ offset)) 8)))

(defun %u32 (bytes offset)
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)
          (ash (aref bytes (+ offset 2)) 16)
          (ash (aref bytes (+ offset 3)) 24)))

(defun %put-u16 (out value)
  (vector-push-extend (logand value #xff) out)
  (vector-push-extend (logand (ash value -8) #xff) out))

(defun %put-u32 (out value)
  (vector-push-extend (logand value #xff) out)
  (vector-push-extend (logand (ash value -8) #xff) out)
  (vector-push-extend (logand (ash value -16) #xff) out)
  (vector-push-extend (logand (ash value -24) #xff) out))

(defun %read-file-bytes (pathname)
  (with-open-file (s pathname :direction :input :element-type '(unsigned-byte 8)
                     :if-does-not-exist :error)
    (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence buf s)
      buf)))

(defun %source-bytes (source)
  (etypecase source
    ((vector (unsigned-byte 8)) source)
    (pathname (%read-file-bytes source))
    (string (%read-file-bytes (pathname source)))
    (stream (%ensure-octets source))))

(defun %zip-decode-name (bytes start len)
  (encoding-protocol:decode (subseq bytes start (+ start len))))

(defun %find-eocd (bytes)
  (let ((len (length bytes)))
    (loop for i from (max 0 (- len 22 65535)) below (- len 21)
          when (and (= (aref bytes i) #x50)
                     (= (aref bytes (+ i 1)) #x4b)
                     (= (aref bytes (+ i 2)) #x05)
                     (= (aref bytes (+ i 3)) #x06))
            do (return i)
          finally (error 'archive-error :message "not a zip archive (EOCD missing)"))))

(defun %load-central-directory (archive)
  (let* ((bytes (zip-archive-bytes archive))
         (eocd (%find-eocd bytes))
         (count (%u16 bytes (+ eocd 10)))
         (cd-offset (%u32 bytes (+ eocd 16)))
         (entries '()))
    (let ((off cd-offset))
      (dotimes (_ count)
        (unless (= (%u32 bytes off) +zip-central-sig+)
          (error 'archive-error :message "corrupt zip central directory"))
        (let* ((method (%u16 bytes (+ off 10)))
               (dos-time (%u16 bytes (+ off 12)))
               (dos-date (%u16 bytes (+ off 14)))
               (crc (%u32 bytes (+ off 16)))
               (comp (%u32 bytes (+ off 20)))
               (uncomp (%u32 bytes (+ off 24)))
               (name-len (%u16 bytes (+ off 28)))
               (extra-len (%u16 bytes (+ off 30)))
               (comment-len (%u16 bytes (+ off 32)))
               (local-off (%u32 bytes (+ off 42)))
               (name (%zip-decode-name bytes (+ off 46) name-len))
               (dir-p (and (plusp (length name))
                           (char= (char name (1- (length name))) #\/))))
          (push (make-instance 'archive-entry
                               :name name
                               :directory-p dir-p
                               :method method
                               :crc crc
                               :compressed-size comp
                               :uncompressed-size uncomp
                               :dos-date dos-date
                               :dos-time dos-time
                               :header-offset local-off)
                entries)
          (incf off (+ 46 name-len extra-len comment-len)))))
    (setf (zip-archive-entry-list archive) (nreverse entries))))

(defun %open-zip-archive (source)
  (let ((archive (make-instance 'zip-archive
                                :format :zip
                                :bytes (%source-bytes source))))
    (%load-central-directory archive)
    archive))

(defmethod archive-entries ((archive zip-archive))
  (copy-list (zip-archive-entry-list archive)))

(defun %find-entry (archive entry)
  (if (archive-entry-p entry)
      entry
      (or (find entry (zip-archive-entry-list archive) :key #'archive-entry-name :test #'string=)
          (let ((slash (concatenate 'string (string-left-trim "/" entry) "/")))
            (find slash (zip-archive-entry-list archive) :key #'archive-entry-name :test #'string=))
          (error 'archive-error :message (format nil "zip entry not found: ~a" entry)))))

(defmethod read-entry ((archive zip-archive) entry)
  (let* ((ent (%find-entry archive entry))
         (bytes (zip-archive-bytes archive))
         (off (archive-entry-header-offset ent)))
    (when (archive-entry-directory-p ent)
      (error 'archive-error :message (format nil "not a file entry: ~a" (archive-entry-name ent))))
    (unless (= (%u32 bytes off) +zip-local-sig+)
      (error 'archive-error :message (format nil "corrupt zip local header: ~a" (archive-entry-name ent))))
    (let* ((name-len (%u16 bytes (+ off 26)))
           (extra-len (%u16 bytes (+ off 28)))
           (data-off (+ off 30 name-len extra-len))
           (comp (subseq bytes data-off (+ data-off (archive-entry-compressed-size ent))))
           (method (archive-entry-method ent)))
      (cond ((= method 0) comp)
            ((= method 8) (decompress comp :algorithm :deflate))
            (t (error 'archive-error
                      :message (format nil "unsupported zip method ~A" method)))))))

(defparameter *crc32-table*
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (dotimes (_ 8)
          (setf c (if (oddp c)
                      (logxor #xedb88320 (ash c -1))
                      (ash c -1))))
        (setf (aref table n) c)))))

(defun %crc32 (bytes)
  (let ((crc #xffffffff))
    (loop for b across bytes
          do (setf crc (logxor (aref *crc32-table* (logand (logxor crc b) #xff))
                               (ash crc -8))))
    (logxor crc #xffffffff)))

(defun %write-zip-bytes (entries)
  "Build a stored (method 0) ZIP as a byte vector.
   ENTRIES is a list of (name string-or-octets)."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (centrals '()))
    (dolist (pair entries)
      (destructuring-bind (name data) pair
        (let* ((name (string-left-trim "/" (string name)))
               (name-octets (encoding-protocol:encode name))
               (payload (if (stringp data)
                            (encoding-protocol:encode data)
                            (coerce data '(vector (unsigned-byte 8)))))
               (crc (%crc32 payload))
               (local-off (length out)))
          (%put-u32 out +zip-local-sig+)
          (%put-u16 out 20)
          (%put-u16 out 0)
          (%put-u16 out 0)
          (%put-u16 out 0) (%put-u16 out 0)
          (%put-u32 out crc)
          (%put-u32 out (length payload))
          (%put-u32 out (length payload))
          (%put-u16 out (length name-octets))
          (%put-u16 out 0)
          (loop for b across name-octets do (vector-push-extend b out))
          (loop for b across payload do (vector-push-extend b out))
          (push (list name-octets crc (length payload) local-off) centrals))))
    (setf centrals (nreverse centrals))
    (let ((cd-off (length out)))
      (dolist (c centrals)
        (destructuring-bind (name-octets crc size local-off) c
          (%put-u32 out +zip-central-sig+)
          (%put-u16 out 20) (%put-u16 out 20)
          (%put-u16 out 0) (%put-u16 out 0)
          (%put-u16 out 0) (%put-u16 out 0)
          (%put-u32 out crc)
          (%put-u32 out size) (%put-u32 out size)
          (%put-u16 out (length name-octets))
          (%put-u16 out 0) (%put-u16 out 0)
          (%put-u16 out 0) (%put-u16 out 0)
          (%put-u32 out 0)
          (%put-u32 out local-off)
          (loop for b across name-octets do (vector-push-extend b out))))
      (let ((cd-size (- (length out) cd-off)))
        (%put-u32 out +zip-eocd-sig+)
        (%put-u16 out 0) (%put-u16 out 0)
        (%put-u16 out (length centrals)) (%put-u16 out (length centrals))
        (%put-u32 out cd-size)
        (%put-u32 out cd-off)
        (%put-u16 out 0)))
    out))
