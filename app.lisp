;; -*- Lisp -*-

(defpackage :app
  (:use :cl :alexandria)
  (:export #:start #:stop
           #:make))

(in-package :app)

(define-condition stop ()
  ((requester :reader requester :initarg :requester)))

(defun wait-for-start ()
  (let ((start-message (messaging:wait-for :code 'start)))
    (cl-log:log-message :info "~S received START message from ~S"
                        (messaging:agent) (messaging:from start-message))
    (messaging:send (ccl:process-name (messaging:from start-message)) 'started)))

(defun wrap-app (function)
  (lambda ()
    (handler-bind
        ((stop (lambda (e)
                 (cl-log:log-message :info "~S received STOP signal from ~S"
                                     (messaging:agent) (requester e))
                 (messaging:send (requester e) 'stopped)
                 (wait-for-start))))
      (progn
        (wait-for-start)
        (funcall function)))))

(defun make (name handler)
  (messaging:make-agent name (wrap-app handler)))

(defvar *current-app* nil)

(defun start (app)
  (stop)
  (handler-case
      (progn
        (cl-log:log-message :info "starting app ~S" app)
        (messaging:send app 'start)
        (messaging:wait-for :code 'started :from app)
        (setf *current-app* app))
    (error (e)
      (cl-log:log-message :error "Cannot start app ~S: ~A" app e)
      (stop))))

(defun stop ()
  (handler-case
      (when *current-app*
        (cl-log:log-message :info "stopping current app ~S" *current-app*)
        (let ((requester (ccl:process-name (messaging:agent))))
          (ccl:process-interrupt (messaging:agent-named *current-app*)
                                 (lambda ()
                                   (signal 'stop :requester requester))))
        (messaging:wait-for :code 'stopped :from *current-app*))
    (messaging:agent-not-found (e)
      (declare (ignore e))))
  (setf *current-app* nil))
