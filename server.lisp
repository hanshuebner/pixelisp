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

(defun start (&key (port 80))
  (setf (cl-log:log-manager)
        (make-instance 'cl-log:log-manager :message-class 'formatted-message))
  (cl-log:start-messenger 'cl-log:text-stream-messenger
                          :stream *standard-output*)
  (setf *random-state* (make-random-state t))
  (storage:start)
  (leds:start-frame-thrower)
  (cl-log:log-message :info "Starting hunchentoot on port ~A" port)
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                    :port port
                                    :document-root #P "html/")))

(defun play-all ()
  (loop
    (cl-log:log-message :info "Scanning GIF directory")
    (let ((gifs (directory #P"gifs/*.gif"))
          (last-update (file-write-date #P"gifs/")))
      (loop
        (let ((gif (alexandria:random-elt gifs)))
          (cl-log:log-message :info "Loading ~A" gif)
          (handler-case
              (leds:send-command :set-animation (leds:load-gif gif))
            (error (e)
              (cl-log:log-message :error "Error loading gif ~A~%~A~%" gif e)
              (sleep .5))))
        (sleep 30)
        (when (/= last-update (file-write-date #P"gifs/"))
          (return))))))

(defun main (command-line-arguments)
  (declare (ignore command-line-arguments))
  (setf ccl:*break-hook* (lambda (cond hook)
                           (declare (ignore cond hook))
                           (format t "Exiting...~%")
                           (ccl:quit)))
  (start)
  (play-all)
  (ccl:join-process ccl:*current-process*))
