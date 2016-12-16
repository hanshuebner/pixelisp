;; -*- Lisp -*-

(defpackage :webserver
  (:use :cl :alexandria)
  (:export #:start))

(in-package :webserver)

(defun start (&key port)
  (cl-log:log-message :info "Starting hunchentoot on port ~A" port)
  (messaging:make-agent :webserver
                        (lambda ()
                          (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                                         :port port
                                                         :document-root #P "html/")))
                            (hunchentoot:start acceptor)
                            (let ((reason (catch 'messaging:exit
                                            (messaging:receive))))
                              (hunchentoot:stop acceptor)
                              (cl-log:log-message :info "Hunchentoot shut down")
                              (throw 'messaging:exit reason))))))
