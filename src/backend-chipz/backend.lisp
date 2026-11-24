(in-package #:compression-backend-chipz)

;;; salza2 mis-encodes zero-length input; use canonical empties.
(defparameter *empty-gzip*
  (coerce #(#x1f #x8b #x08 #x00 #x00 #x00 #x00 #x00 #x00 #xff
            #x03 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
          '(simple-array (unsigned-byte 8) (*))))

(defparameter *empty-zlib*
  (coerce #(#x78 #x9c #x03 #x00 #x00 #x00 #x00 #x01)
          '(simple-array (unsigned-byte 8) (*))))

(defparameter *empty-deflate*
  (coerce #(#x03 #x00)
          '(simple-array (unsigned-byte 8) (*))))

(defun %chipz-format (algorithm)
  (ecase algorithm
    ((:gzip :x-gzip) 'chipz:gzip)
    ((:zlib) 'chipz:zlib)
    ((:deflate) 'chipz:deflate)))

(defun %salza-compressor (algorithm)
  (ecase algorithm
    ((:gzip :x-gzip) 'salza2:gzip-compressor)
    ((:zlib) 'salza2:zlib-compressor)
    ((:deflate) 'salza2:deflate-compressor)))

(defun %empty (algorithm)
  (ecase algorithm
    ((:gzip :x-gzip) *empty-gzip*)
    ((:zlib) *empty-zlib*)
    ((:deflate) *empty-deflate*)))

(defun %compress (algorithm data)
  (let ((octets (compression-protocol::%ensure-octets data)))
    (if (zerop (length octets))
        (%empty algorithm)
        (salza2:compress-data octets (%salza-compressor algorithm)))))

;;; Data chipz cannot decode signals COMPRESSION-ERROR with chipz's own
;;; condition as its CAUSE, so callers handle one protocol type (Python:
;;; zlib.error / OSError). chipz signals CHIPZ-ERROR subclasses for every
;;; decode failure it detects.

(defun %corrupt (algorithm condition)
  (error 'compression-error
         :algorithm algorithm
         :message (format nil "corrupt ~(~a~) data: ~a" algorithm condition)
         :cause condition))

(defmacro %translating-chipz-errors ((algorithm) &body body)
  `(handler-case (progn ,@body)
     (chipz:chipz-error (c) (%corrupt ,algorithm c))))

(defun %decompress (algorithm data)
  (let ((octets (compression-protocol::%ensure-octets data)))
    (%translating-chipz-errors (algorithm)
      (if (member algorithm '(:bzip2 :bz2))
          (chipz:decompress nil 'chipz:bzip2 octets)
          (chipz:decompress nil (%chipz-format algorithm) octets)))))

(defclass chipz-input-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((inner :initarg :inner :reader %inner)
   (algorithm :initarg :algorithm :reader %algorithm))
  (:documentation "A chipz decompressing stream whose read errors signal
COMPRESSION-ERROR."))

(defmethod stream-element-type ((s chipz-input-stream)) '(unsigned-byte 8))

(defmethod trivial-gray-streams:stream-read-byte ((s chipz-input-stream))
  (%translating-chipz-errors ((%algorithm s))
    (read-byte (%inner s) nil :eof)))

(defmethod trivial-gray-streams:stream-read-sequence
    ((s chipz-input-stream) sequence start end &key)
  (%translating-chipz-errors ((%algorithm s))
    (read-sequence sequence (%inner s) :start start :end (or end (length sequence)))))

(defmethod close ((s chipz-input-stream) &key abort)
  (close (%inner s) :abort abort)
  (call-next-method))

(defun %decompressing-stream (algorithm input)
  (make-instance 'chipz-input-stream
                 :algorithm algorithm
                 :inner (%translating-chipz-errors (algorithm)
                          (chipz:make-decompressing-stream
                           (if (member algorithm '(:bzip2 :bz2))
                               'chipz:bzip2
                               (%chipz-format algorithm))
                           input))))

(macrolet ((define-chipz-codec (algorithm)
             `(progn
                (defmethod compress-using-algorithm ((algorithm (eql ,algorithm)) data &key level)
                  (declare (ignore level))
                  (%compress ,algorithm data))
                (defmethod decompress-using-algorithm ((algorithm (eql ,algorithm)) data &key)
                  (%decompress ,algorithm data))
                (defmethod make-decompressing-stream-using-algorithm
                    ((algorithm (eql ,algorithm)) input &key)
                  (%decompressing-stream ,algorithm input))
                (defmethod make-compressing-stream-using-algorithm
                    ((algorithm (eql ,algorithm)) output &key level)
                  (declare (ignore level))
                  (salza2:make-compressing-stream (%salza-compressor ,algorithm) output)))))
  (define-chipz-codec :gzip)
  (define-chipz-codec :x-gzip)
  (define-chipz-codec :zlib)
  (define-chipz-codec :deflate))

(defmethod decompress-using-algorithm ((algorithm (eql :bzip2)) data &key)
  (%decompress :bzip2 data))

(defmethod decompress-using-algorithm ((algorithm (eql :bz2)) data &key)
  (%decompress :bz2 data))

(defmethod make-decompressing-stream-using-algorithm ((algorithm (eql :bzip2)) input &key)
  (%decompressing-stream :bzip2 input))

(defmethod make-decompressing-stream-using-algorithm ((algorithm (eql :bz2)) input &key)
  (%decompressing-stream :bz2 input))

(defun use-chipz-backend ()
  (setf *compression-backend* :chipz)
  :chipz)

(use-chipz-backend)
