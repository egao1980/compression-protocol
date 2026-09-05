(defpackage #:compression-protocol
  (:use #:cl)
  (:nicknames #:stack-compression)
  (:export #:compression-error
           #:unsupported-algorithm
           #:compression-error-message
           #:compression-error-algorithm
           #:archive-error

           #:*compression-backend*

           #:compress
           #:decompress
           #:make-compressing-stream
           #:make-decompressing-stream

           #:archive
           #:archive-format
           #:archive-entry
           #:archive-entry-p
           #:archive-entry-name
           #:archive-entry-directory-p
           #:archive-entry-method
           #:archive-entry-crc
           #:archive-entry-compressed-size
           #:archive-entry-uncompressed-size
           #:archive-entry-dos-date
           #:archive-entry-dos-time

           #:open-archive
           #:close-archive
           #:archive-entries
           #:read-entry
           #:write-entry
           #:write-archive-bytes))

(in-package #:compression-protocol)
