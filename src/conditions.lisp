(in-package #:compression-protocol)

(define-condition compression-error (error)
  ((message :initarg :message :reader compression-error-message :initform nil)
   (algorithm :initarg :algorithm :reader compression-error-algorithm :initform nil)
   (cause :initarg :cause :reader compression-error-cause :initform nil))
  (:report (lambda (c s)
             (format s "compression error~@[ (~s)~]~@[: ~a~]"
                     (compression-error-algorithm c)
                     (compression-error-message c))))
  (:documentation "Root of compression-protocol's conditions. A backend
signals it (or a subclass) for data it cannot decode; CAUSE is the backend
library's own condition when it wraps one (chipz's DECOMPRESSION-ERROR)."))

(define-condition unsupported-algorithm (compression-error) ())

(define-condition compression-limit-exceeded (compression-error)
  ((limit :initarg :limit :reader compression-limit-exceeded-limit))
  (:report (lambda (c s)
             (format s "decompressed data~@[ (~s)~] exceeds ~:d bytes"
                     (compression-error-algorithm c)
                     (compression-limit-exceeded-limit c)))))

(define-condition archive-error (compression-error) ()
  (:documentation "A damaged or unsupported archive: a structure past the end
of the data, a bad signature, an entry whose CRC-32 or size does not match
its header."))
