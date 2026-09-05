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

(defmethod compress (data &key (algorithm :gzip) level)
  (declare (ignore level))
  (unless (member algorithm '(:gzip :x-gzip :zlib :deflate))
    (error 'unsupported-algorithm :algorithm algorithm
           :message (format nil "chipz backend cannot compress ~s" algorithm)))
  (let ((octets (compression-protocol::%ensure-octets data)))
    (if (zerop (length octets))
        (%empty algorithm)
        (salza2:compress-data octets (%salza-compressor algorithm)))))

(defmethod decompress (data &key (algorithm :gzip))
  (unless (member algorithm '(:gzip :x-gzip :zlib :deflate))
    (error 'unsupported-algorithm :algorithm algorithm
           :message (format nil "chipz backend cannot decompress ~s" algorithm)))
  (chipz:decompress nil (%chipz-format algorithm)
                    (compression-protocol::%ensure-octets data)))

(defmethod make-decompressing-stream (input &key (algorithm :gzip))
  (unless (member algorithm '(:gzip :x-gzip :zlib :deflate))
    (error 'unsupported-algorithm :algorithm algorithm
           :message (format nil "chipz backend cannot decompress ~s" algorithm)))
  (chipz:make-decompressing-stream (%chipz-format algorithm) input))

(defmethod make-compressing-stream (output &key (algorithm :gzip) level)
  (declare (ignore level))
  (unless (member algorithm '(:gzip :x-gzip :zlib :deflate))
    (error 'unsupported-algorithm :algorithm algorithm
           :message (format nil "chipz backend cannot compress ~s" algorithm)))
  (salza2:make-compressing-stream (%salza-compressor algorithm) output))

(defun use-chipz-backend ()
  (setf *compression-backend* :chipz)
  :chipz)

(use-chipz-backend)
