(in-package #:compression-protocol)

(define-condition compression-error (error)
  ((message :initarg :message :reader compression-error-message :initform nil)
   (algorithm :initarg :algorithm :reader compression-error-algorithm :initform nil))
  (:report (lambda (c s)
             (format s "compression error~@[ (~s)~]~@[: ~a~]"
                     (compression-error-algorithm c)
                     (compression-error-message c)))))

(define-condition unsupported-algorithm (compression-error) ())

(define-condition compression-limit-exceeded (compression-error)
  ((limit :initarg :limit :reader compression-limit-exceeded-limit))
  (:report (lambda (c s)
             (format s "decompressed data~@[ (~s)~] exceeds ~:d bytes"
                     (compression-error-algorithm c)
                     (compression-limit-exceeded-limit c)))))

(define-condition archive-error (compression-error) ())
