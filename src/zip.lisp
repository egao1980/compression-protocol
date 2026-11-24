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

(defun %zip-decode-name (bytes start len flags)
  "The entry name at START: UTF-8 when general-purpose bit 11 is set in
FLAGS (invalid UTF-8 there signals ARCHIVE-ERROR), else UTF-8 when it
decodes, else ISO-8859-1, which never fails. The zip default is CP437
(APPNOTE appendix D; Python's zipfile reads every unflagged name so), but
encoding-protocol has no CP437 yet (egao1980/encoding-protocol#4)."
  (let ((octets (subseq bytes start (+ start len))))
    (handler-case (encoding-protocol:decode octets :encoding :utf-8 :strict t)
      (encoding-protocol:encoding-error (e)
        (if (logbitp 11 flags)
            (error 'archive-error
                   :message (format nil "zip entry name is not UTF-8: ~a" e)
                   :cause e)
            (encoding-protocol:decode octets :encoding :iso-8859-1))))))

(defun %zip-need (bytes offset size what)
  "Signal ARCHIVE-ERROR unless the SIZE bytes of WHAT at OFFSET lie inside
BYTES (Python's zipfile: BadZipFile)."
  ;; One-line control string: ~ before a newline breaks on a CRLF checkout.
  (unless (<= (+ offset size) (length bytes))
    (error 'archive-error
           :message (format nil "corrupt zip: ~a runs past the end of the archive (~:d bytes at ~:d, archive ~:d bytes)"
                            what size offset (length bytes)))))

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
  "Read ARCHIVE's central directory into its entry list. A record that runs
past the end of the data, a bad signature or a ZIP64 marker (#xFFFFFFFF
size or offset) signals ARCHIVE-ERROR."
  (let* ((bytes (zip-archive-bytes archive))
         (eocd (%find-eocd bytes))
         (count (%u16 bytes (+ eocd 10)))
         (cd-offset (%u32 bytes (+ eocd 16)))
         (entries '()))
    (let ((off cd-offset))
      (dotimes (_ count)
        (%zip-need bytes off 46 "central directory entry")
        (unless (= (%u32 bytes off) +zip-central-sig+)
          (error 'archive-error :message "corrupt zip central directory"))
        (let* ((flags (%u16 bytes (+ off 8)))
               (method (%u16 bytes (+ off 10)))
               (dos-time (%u16 bytes (+ off 12)))
               (dos-date (%u16 bytes (+ off 14)))
               (crc (%u32 bytes (+ off 16)))
               (comp (%u32 bytes (+ off 20)))
               (uncomp (%u32 bytes (+ off 24)))
               (name-len (%u16 bytes (+ off 28)))
               (extra-len (%u16 bytes (+ off 30)))
               (comment-len (%u16 bytes (+ off 32)))
               (local-off (%u32 bytes (+ off 42)))
               (name (progn
                       (%zip-need bytes (+ off 46) (+ name-len extra-len comment-len)
                                  "central directory entry")
                       (%zip-decode-name bytes (+ off 46) name-len flags)))
               (dir-p (and (plusp (length name))
                           (char= (char name (1- (length name))) #\/))))
          (when (member #xffffffff (list comp uncomp local-off))
            (error 'archive-error
                   :message (format nil "ZIP64 entries are not supported: ~a" name)))
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

(defun %check-zip-entry (entry data)
  "DATA, the bytes read for ENTRY, when their length and CRC-32 match the
central directory. Otherwise ARCHIVE-ERROR, as Python's zipfile raises
BadZipFile (\"Bad CRC-32\"); its CONTINUE restart returns DATA anyway."
  (let ((name (archive-entry-name entry))
        (size (archive-entry-uncompressed-size entry))
        (crc (archive-entry-crc entry)))
    (flet ((bad (message)
             (restart-case (error 'archive-error :message message)
               (continue ()
                 :report "Use the entry's bytes anyway"
                 (return-from %check-zip-entry data)))))
      (cond ((/= (length data) size)
             (bad (format nil "zip entry ~a is ~:d bytes, its header says ~:d"
                          name (length data) size)))
            ((/= (%crc32 data) crc)
             (bad (format nil "bad CRC-32 for zip entry ~a: #x~8,'0x, header #x~8,'0x"
                          name (%crc32 data) crc))))
      data)))

(defmethod read-entry ((archive zip-archive) entry &key max-size)
  (let* ((ent (%find-entry archive entry))
         (name (archive-entry-name ent))
         (bytes (zip-archive-bytes archive))
         (off (archive-entry-header-offset ent))
         (method (archive-entry-method ent)))
    (when (archive-entry-directory-p ent)
      (error 'archive-error :message (format nil "not a file entry: ~a" name)))
    (%zip-need bytes off 30 (format nil "local header of ~a" name))
    (unless (= (%u32 bytes off) +zip-local-sig+)
      (error 'archive-error :message (format nil "corrupt zip local header: ~a" name)))
    (unless (member method '(0 8))
      (error 'archive-error :message (format nil "unsupported zip method ~A" method)))
    (when (and max-size
               (> (max (archive-entry-uncompressed-size ent)
                       (if (= method 0) (archive-entry-compressed-size ent) 0))
                  max-size))
      (error 'compression-limit-exceeded :algorithm :zip :limit max-size))
    (let* ((name-len (%u16 bytes (+ off 26)))
           (extra-len (%u16 bytes (+ off 28)))
           (data-off (+ off 30 name-len extra-len))
           (comp-size (archive-entry-compressed-size ent))
           (comp (progn (%zip-need bytes data-off comp-size (format nil "data of ~a" name))
                   (subseq bytes data-off (+ data-off comp-size)))))
      (%check-zip-entry
       ent
       (cond ((= method 0) comp)
             ;; MAX-SIZE bounds the inflated bytes themselves, so a header
             ;; that understates them cannot inflate a bomb into memory.
             (max-size (decompress comp :algorithm :deflate :max-output max-size))
             (t (decompress comp :algorithm :deflate)))))))

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
  "CRC-32 (ISO 3309, as zip and gzip) of the octet vector BYTES."
  (let ((octets (if (typep bytes '(simple-array (unsigned-byte 8) (*)))
                    bytes
                    (coerce bytes '(simple-array (unsigned-byte 8) (*)))))
        (table *crc32-table*)
        (crc #xffffffff))
    (declare (type (simple-array (unsigned-byte 8) (*)) octets)
             (type (simple-array (unsigned-byte 32) (256)) table)
             (type (unsigned-byte 32) crc)
             (optimize speed))
    (loop for b of-type (unsigned-byte 8) across octets
          do (setf crc (logxor (aref table (logand (logxor crc b) #xff))
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
