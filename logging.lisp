;; -*- Lisp -*-

(defpackage :logging
  (:use :cl :alexandria)
  (:export #:start))

(in-package :logging)

(defclass formatted-message (cl-log:formatted-message)
  ())

(defun format-log-timestamp (timestamp)
  (local-time:format-timestring nil
                                (local-time:universal-to-timestamp (cl-log:timestamp-universal-time timestamp))
                                :format '(:year "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2))))

(defmethod cl-log:format-message ((self formatted-message))
    (format nil "~A: ~A ~?~&"
            (format-log-timestamp (cl-log:message-timestamp self))
            (cl-log:message-category self)
            (cl-log:message-description self)
            (cl-log:message-arguments self)))

(defun start ()
  (setf (cl-log:log-manager)
        (make-instance 'cl-log:log-manager :message-class 'formatted-message))
  (cl-log:start-messenger 'cl-log:text-stream-messenger
                          :stream *standard-output*))
