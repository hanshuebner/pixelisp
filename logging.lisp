;; -*- Lisp -*-

(defpackage :logging
  (:use :cl :alexandria)
  (:export #:start
           #:last-messages
           #:format-log-timestamp))

(in-package :logging)

(defclass formatted-message (cl-log:formatted-message)
  ())

(defun format-log-timestamp (timestamp)
  (local-time:format-timestring nil
                                (local-time:universal-to-timestamp (cl-log:timestamp-universal-time timestamp))
                                :format '(:year "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2))))

(defmethod cl-log:format-message ((message formatted-message))
  (format nil "~A: ~A [~A] ~?~&"
          (format-log-timestamp (cl-log:message-timestamp message))
          (cl-log:message-category message)
          (ccl:process-name ccl:*current-process*)
          (cl-log:message-description message)
          (cl-log:message-arguments message)))

(defun start ()
  (setf (cl-log:log-manager)
        (make-instance 'cl-log:log-manager :message-class 'formatted-message))
  (cl-log:start-messenger 'cl-log:text-stream-messenger
                          :name :console
                          :stream *standard-output*)
  (cl-log:start-messenger 'cl-log:ring-messenger
                          :name :log-ring
                          :length 100)
  (let ((stream (open "game-frame.log"
                      :direction :output
                      :element-type :default
                      :if-does-not-exist :create
                      :if-exists :append
                      :sharing :lock)))
    (cl-log:start-messenger 'cl-log:text-stream-messenger
                            :name :logfile
                            :stream stream)))

(defun last-messages ()
  (cl-log:ring-messenger-messages (cl-log:find-messenger :log-ring)))
