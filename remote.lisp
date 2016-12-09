;; -*- Lisp -*-

(defpackage :remote
  (:use :cl :alexandria)
  (:export #:start))

(in-package :remote)

(defparameter *remote-port* 2020)

(defun parse-message (string)
  (multiple-value-bind (match registers)
      (ppcre:scan-to-strings "^[0-9a-f]+ 00 ([^ ]*) .*" string)
    (when match
      (intern (ppcre:regex-replace-all "_" (svref registers 0) "-") :keyword))))

(defun start (agent)
  (erlangen:spawn (lambda ()
                    (erlangen:register :remote-control-reader)
                    (let ((socket (ccl:make-socket :remote-host "localhost" :remote-port *remote-port*)))
                      (unwind-protect
                           (loop
                             (when-let (key (parse-message (read-line socket)))
                               (erlangen:send (list :key-pressed key) agent)))
                        (close socket))))))
