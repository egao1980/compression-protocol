(in-package #:compression-protocol)

;;; A minimal binary input stream over an octet vector, so a vector can be fed
;;; to a backend's decompressing stream.

(defclass octet-vector-input-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((octets :initarg :octets :reader %octets)
   (position :initform 0 :accessor %position)))

(defmethod stream-element-type ((s octet-vector-input-stream)) '(unsigned-byte 8))

(defmethod trivial-gray-streams:stream-read-byte ((s octet-vector-input-stream))
  (let ((p (%position s)) (v (%octets s)))
    (if (< p (length v))
        (prog1 (aref v p) (setf (%position s) (1+ p)))
        :eof)))

(defmethod trivial-gray-streams:stream-read-sequence
    ((s octet-vector-input-stream) sequence start end &key)
  (let* ((v (%octets s)) (p (%position s))
         (n (max 0 (min (- (or end (length sequence)) start) (- (length v) p)))))
    (replace sequence v :start1 start :end1 (+ start n) :start2 p)
    (setf (%position s) (+ p n))
    (+ start n)))

(defun %octet-input (octets)
  (make-instance 'octet-vector-input-stream :octets octets))
