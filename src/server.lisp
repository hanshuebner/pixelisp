;; -*- Lisp -*-

(defpackage :server
  (:use :cl :alexandria)
  (:export #:start))

(in-package :server)

(defparameter *default-http-port* #+darwin 8899 #-darwin 80)

(defun start (&key (port *default-http-port*))
  (logging:start)
  (messaging:make-agent :main
                        (lambda ()
                          (storage:start)
                          (display:start)
                          (when port
                            (webserver:start :port port))
                          (remote-control:start)
                          (app:make :clock 'clock:run)
                          (app:make :gallery 'gallery:play)
                          (controller:start)
                          (loop
                            (let ((message (messaging:receive)))
                              (cl-log:log-message :info "received message ~S" message))))
                        :parent nil))

(defun main (command-line-arguments)
  (declare (ignore command-line-arguments))
  (setf ccl:*break-hook* (lambda (cond hook)
                           (declare (ignore cond hook))
                           (format t "Exiting...~%")
                           (ccl:quit)))
  (start)
  (ccl:join-process ccl:*current-process*))
