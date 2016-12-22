;; -*- Lisp -*-

(defpackage :webserver
  (:use :cl :alexandria)
  (:export #:start))

(in-package :webserver)

(defclass acceptor (hunchentoot:easy-acceptor)
  ())

(defmethod hunchentoot:acceptor-log-access ((acceptor acceptor) &key return-code)
  (cl-log:log-message :info
                      "(~A~@[ ~A~]) ~A ~A~@[?~A~] => ~A~@[ (~A bytes)~]"
                      (hunchentoot:remote-addr*)
                      (hunchentoot:authorization)
                      (hunchentoot:request-method*)
                      (hunchentoot:script-name*)
                      (hunchentoot:query-string*)
                      return-code
                      (hunchentoot:content-length*)))

(defmethod hunchentoot:acceptor-log-message ((acceptor acceptor) log-level format-string &rest format-arguments)
  (cl-log:log-message :error "~S ~?" log-level format-string format-arguments))

(defun start (&key port)
  (cl-log:log-message :info "Starting hunchentoot on port ~A" port)
  (messaging:make-agent :webserver
                        (lambda ()
                          (let ((acceptor (make-instance 'acceptor
                                                         :port port
                                                         :document-root #P "html/")))
                            (hunchentoot:start acceptor)
                            (let ((reason (catch 'messaging:exit
                                            (messaging:receive))))
                              (hunchentoot:stop acceptor)
                              (cl-log:log-message :info "Hunchentoot shut down")
                              (throw 'messaging:exit reason))))))
