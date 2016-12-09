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

(defun play-all ()
  (loop
    (cl-log:log-message :info "Scanning GIF directory")
    (let ((gifs (directory #P"gifs/*.gif"))
          (last-update (file-write-date #P"gifs/")))
      (loop
        (let ((gif (alexandria:random-elt gifs)))
          (cl-log:log-message :info "Loading ~A" gif)
          (handler-case
              (erlangen:send (list :set-animation (display:load-gif gif)) :display)
            (error (e)
              (cl-log:log-message :error "Error loading gif ~A~%~A~%" gif e)
              (sleep .5))))
        (utils:sleep 30)
        (when (/= last-update (file-write-date #P"gifs/"))
          (return))))))

(defun wait-for-key (key)
  (loop
    (let ((message (erlangen:receive)))
      (cl-log:log-message :info "received message ~A" message)
      (when (equal message (list :key-pressed key))
        (return)))))

(defun start (&key (port 80))
  (setf (cl-log:log-manager)
        (make-instance 'cl-log:log-manager :message-class 'formatted-message))
  (cl-log:start-messenger 'cl-log:text-stream-messenger
                          :stream *standard-output*)
  (setf *random-state* (make-random-state t))
  (storage:start)
  (display:start)
  (cl-log:log-message :info "Starting hunchentoot on port ~A" port)
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                    :port port
                                    :document-root #P "html/"))
  (erlangen:spawn (lambda ()
                    (erlangen:register :main)
                    (remote:start :main)
                    (loop
                      (erlangen:spawn (lambda ()
                                        (erlangen:register :player)
                                        (play-all)))
                      (wait-for-key :key-power)
                      (erlangen:exit 'stopped :player)
                      (erlangen:unregister :player)
                      (erlangen:send (list :blank) :display)
                      (wait-for-key :key-power)))))

(defun main (command-line-arguments)
  (declare (ignore command-line-arguments))
  (setf ccl:*break-hook* (lambda (cond hook)
                           (declare (ignore cond hook))
                           (format t "Exiting...~%")
                           (ccl:quit)))
  (start)
  (play-all)
  (ccl:join-process ccl:*current-process*))
