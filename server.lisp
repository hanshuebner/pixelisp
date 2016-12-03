;; -*- Lisp -*-

(defpackage :server
  (:use :cl :alexandria))

(in-package :server)

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

(defvar *led-commands-queue*)

(defun start (&key (port 8428))
  (setf (cl-log:log-manager)
        (make-instance 'cl-log:log-manager :message-class 'formatted-message))
  (cl-log:start-messenger 'cl-log:text-stream-messenger
                          :stream *standard-output*)
  (setf *led-commands-queue* (queues:make-queue :simple-cqueue))
  (leds:start-frame-thrower *led-commands-queue*)
  (cl-log:log-message :info "Starting hunchentoot on port ~A" port)
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                    :port port
                                    :document-root #P "html/")))

(defun play-all ()
  (loop
    (dolist (gif (directory #P"gifs/*.gif"))
      (handler-case
          (queues:qpush *led-commands-queue* `(:set-animation ,(leds:load-gif gif)))
        (error (e)
          (format t "error loading gif ~A~%~A~%" gif e)
          (sleep .5)))
      (sleep 10))))

(defun main (command-line-arguments)
  (declare (ignore command-line-arguments))
  (setf ccl:*break-hook* (lambda (cond hook)
                           (declare (ignore cond hook))
                           (format t "Exiting...~%")
                           (ccl:quit)))
  (start)
  (play-all)
  (bt:join-thread (bt:current-thread)))
