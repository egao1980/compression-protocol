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

(defun %decompress (algorithm data)
  (chipz:decompress nil (%chipz-format algorithm)
                    (compression-protocol::%ensure-octets data)))

(macrolet ((define-chipz-codec (algorithm)
             `(progn
                (defmethod compress-using-algorithm ((algorithm (eql ,algorithm)) data &key level)
                  (declare (ignore level))
                  (%compress ,algorithm data))
                (defmethod decompress-using-algorithm ((algorithm (eql ,algorithm)) data &key)
                  (%decompress ,algorithm data))
                (defmethod make-decompressing-stream-using-algorithm
                    ((algorithm (eql ,algorithm)) input &key)
                  (chipz:make-decompressing-stream (%chipz-format ,algorithm) input))
                (defmethod make-compressing-stream-using-algorithm
                    ((algorithm (eql ,algorithm)) output &key level)
                  (declare (ignore level))
                  (salza2:make-compressing-stream (%salza-compressor ,algorithm) output)))))
  (define-chipz-codec :gzip)
  (define-chipz-codec :x-gzip)
  (define-chipz-codec :zlib)
  (define-chipz-codec :deflate))

(defun use-chipz-backend ()
  (setf *compression-backend* :chipz)
  :chipz)

(use-chipz-backend)
