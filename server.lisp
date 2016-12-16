;; -*- Lisp -*-

(defpackage :server
  (:use :cl :alexandria)
  (:export
   #:start))

(in-package :server)

(defun start (&key (port 80))
  (logging:start)
  (messaging:make-agent :main
                        (lambda ()
                          (storage:start)
                          (display:start)
                          (when port
                            (webserver:start :port port))
                          (scripter:start)
                          (remote-control:start :scripter)
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
