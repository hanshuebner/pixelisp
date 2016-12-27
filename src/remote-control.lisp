;; -*- Lisp -*-

(defpackage :remote-control
  (:use :cl :alexandria)
  (:export #:start))

(in-package :remote-control)

(defparameter *remote-control-host* "localhost")
(defparameter *remote-control-port* 2020)

(defun parse-message (string)
  (multiple-value-bind (match registers)
      (ppcre:scan-to-strings "^[0-9a-f]+ 00 ([^ ]*) .*" string)
    (when match
      (intern (ppcre:regex-replace-all "_" (svref registers 0) "-") :keyword))))

(defun start ()
  (messaging:make-agent :remote-control-reader
                        (lambda ()
                          (handler-case
                              (let ((socket (ccl:make-socket :remote-host *remote-control-host* :remote-port *remote-control-port*)))
                                (unwind-protect
                                     (loop
                                       (when-let (key (parse-message (read-line socket)))
                                         (setf (controller:power) :toggle)))
                                  (close socket)))
                            (ccl:socket-creation-error (e)
                              (if (eql (ccl:socket-creation-error-identifier e) :connection-refused)
                                  (cl-log:log-message :error "Connection to remote control server on ~A:~A refused"
                                                      *remote-control-host* *remote-control-port*)
                                  (cl-log:log-message :error "Unexpected error ~A while connecting to remote control server on ~A:~A"
                                                      e *remote-control-host* *remote-control-port*)))))))
