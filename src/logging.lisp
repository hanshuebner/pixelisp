;; -*- Lisp -*-

(defpackage :logging
  (:use :cl :alexandria)
  (:export #:start
           #:last-messages
           #:format-log-timestamp))

(in-package :logging)

(defparameter *logfile-name* #P"log/pixelisp.log")

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

(defclass log-file-messenger (cl-log:text-stream-messenger)
  ((filename :reader filename
             :initarg :filename)
   (external-format :reader external-format
                    :initarg :external-format
                    :initform :default)
   (log-file-size :accessor log-file-size
                  :initform most-positive-fixnum)))

(defun file-size (filename)
  (nth-value 2 (ccl::%stat (namestring filename))))

(defun reopen (messenger)
  (let ((stream (open (filename messenger)
                      :direction :output
                      :element-type :default
                      :if-does-not-exist :create
                      :if-exists :append
                      :sharing :lock
                      :external-format (external-format messenger))))
    (setf (slot-value messenger 'cl-log::stream) stream
          (log-file-size messenger) (file-size (filename messenger)))))

(defmethod initialize-instance :after ((messenger log-file-messenger) &key)
  (reopen messenger))

(defmethod cl-log:messenger-send-message :before ((messenger log-file-messenger) (message t))
  (let ((current-log-file-size (file-size (filename messenger))))
    (when (or (null current-log-file-size)
              (< current-log-file-size (log-file-size messenger)))
      (reopen messenger))))

(defun start ()
  (setf (cl-log:log-manager)
        (make-instance 'cl-log:log-manager :message-class 'formatted-message))
  (cl-log:start-messenger 'cl-log:text-stream-messenger
                          :name :console
                          :stream *standard-output*)
  (cl-log:start-messenger 'cl-log:ring-messenger
                          :name :log-ring
                          :length 100)
  (cl-log:start-messenger 'log-file-messenger
                          :name :logfile
                          :filename *logfile-name*))

(defun last-messages ()
  (cl-log:ring-messenger-messages (cl-log:find-messenger :log-ring)))
