(in-package #:compression-protocol)

(defvar *compression-backend* :chipz
  "Hint for which codec implementation is loaded. Methods live on ALGORITHM.")

(defun %ensure-octets (data)
  (cond
    ((and (vectorp data) (not (stringp data)))
     (coerce data '(simple-array (unsigned-byte 8) (*))))
    ((stringp data) (encoding-protocol:encode data))
    ((streamp data)
     (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0))
           (buf (make-array 4096 :element-type '(unsigned-byte 8))))
       (loop for n = (read-sequence buf data)
             while (plusp n)
             do (loop for i below n do (vector-push-extend (aref buf i) out)))
       (coerce out '(simple-array (unsigned-byte 8) (*)))))
    (t (error 'compression-error
              :message (format nil "not octets, string, or stream: ~s" (type-of data))))))

(defgeneric compress-using-algorithm (algorithm data &key level)
  (:documentation "Backend GF. Specialize ALGORITHM with EQL. Do not specialize COMPRESS."))

(defgeneric decompress-using-algorithm (algorithm data &key)
  (:documentation "Backend GF. Specialize ALGORITHM with EQL. Do not specialize DECOMPRESS."))

(defgeneric make-compressing-stream-using-algorithm (algorithm output &key level)
  (:documentation "Backend GF. Push compressed bytes into OUTPUT."))

(defgeneric make-decompressing-stream-using-algorithm (algorithm input &key)
  (:documentation "Backend GF. Pull decompressed bytes from INPUT."))

(defmethod compress-using-algorithm (algorithm data &key level)
  (declare (ignore data level))
  (error 'unsupported-algorithm
         :algorithm algorithm
         :message (format nil "no compression backend for ~s" algorithm)))

(defmethod decompress-using-algorithm (algorithm data &key)
  (declare (ignore data))
  (error 'unsupported-algorithm
         :algorithm algorithm
         :message (format nil "no decompression backend for ~s" algorithm)))

(defmethod make-compressing-stream-using-algorithm (algorithm output &key level)
  (declare (ignore output level))
  (error 'unsupported-algorithm
         :algorithm algorithm
         :message (format nil "no compressing stream for ~s" algorithm)))

(defmethod make-decompressing-stream-using-algorithm (algorithm input &key)
  (declare (ignore input))
  (error 'unsupported-algorithm
         :algorithm algorithm
         :message (format nil "no decompressing stream for ~s" algorithm)))

(defun compress (data &key (algorithm :gzip) level)
  "Compress DATA (octets, string, or stream) with ALGORITHM.
   ALGORITHM is :gzip, :deflate, :zlib, :br, :zstd, or :snappy."
  (compress-using-algorithm algorithm data :level level))

(defun decompress (data &key (algorithm :gzip))
  "Decompress DATA (octets or stream) with ALGORITHM."
  (decompress-using-algorithm algorithm data))

(defun make-compressing-stream (output &key (algorithm :gzip) level)
  "Return a binary output stream: writes are compressed into OUTPUT."
  (make-compressing-stream-using-algorithm algorithm output :level level))

(defun make-decompressing-stream (input &key (algorithm :gzip))
  "Return a binary input stream of decompressed bytes from INPUT."
  (make-decompressing-stream-using-algorithm algorithm input))

(defclass archive ()
  ((format :initarg :format :reader archive-format)
   (closed :initform nil :accessor archive-closed-p)))

(defclass archive-entry ()
  ((name :initarg :name :reader archive-entry-name)
   (directory-p :initarg :directory-p :reader archive-entry-directory-p :initform nil)
   (method :initarg :method :reader archive-entry-method :initform 0)
   (crc :initarg :crc :reader archive-entry-crc :initform 0)
   (compressed-size :initarg :compressed-size :reader archive-entry-compressed-size :initform 0)
   (uncompressed-size :initarg :uncompressed-size :reader archive-entry-uncompressed-size :initform 0)
   (dos-date :initarg :dos-date :reader archive-entry-dos-date :initform 0)
   (dos-time :initarg :dos-time :reader archive-entry-dos-time :initform 0)
   (header-offset :initarg :header-offset :reader archive-entry-header-offset :initform 0)))

(defun archive-entry-p (x) (typep x 'archive-entry))

(defgeneric open-archive (source &key format)
  (:documentation "Open SOURCE (pathname, namestring, or octet vector) as an archive.
   FORMAT defaults to :zip."))

(defgeneric close-archive (archive)
  (:documentation "Release archive resources. Idempotent."))

(defgeneric archive-entries (archive)
  (:documentation "List of ARCHIVE-ENTRY objects."))

(defgeneric read-entry (archive entry)
  (:documentation "Return uncompressed octets for ENTRY (an ARCHIVE-ENTRY or name string)."))

(defgeneric write-entry (archive name data &key)
  (:documentation "Append NAME → DATA to a writable archive. Optional for read-only formats."))

(defmethod close-archive ((archive archive))
  (setf (archive-closed-p archive) t)
  archive)

(defmethod write-entry (archive name data &key)
  (declare (ignore name data))
  (error 'archive-error
         :message (format nil "write-entry not supported for ~s" (archive-format archive))))
