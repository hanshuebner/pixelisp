;; -*- Lisp -*-

(defpackage :remote-control
  (:use :cl :alexandria)
  (:export #:start))

(in-package :remote-control)

(defparameter *remote-control-port* 2020)

(defun parse-message (string)
  (multiple-value-bind (match registers)
      (ppcre:scan-to-strings "^[0-9a-f]+ 00 ([^ ]*) .*" string)
    (when match
      (intern (ppcre:regex-replace-all "_" (svref registers 0) "-") :keyword))))

(defun start (recipient)
  (messaging:make-agent :remote-control-reader
                        (lambda ()
                          (let ((socket (ccl:make-socket :remote-host "localhost" :remote-port *remote-control-port*)))
                            (unwind-protect
                                 (loop
                                   (when-let (key (parse-message (read-line socket)))
                                     (messaging:send recipient :key-pressed key)))
                              (close socket))))))
