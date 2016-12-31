;; -*- Lisp -*-

(defpackage :app
  (:use :cl :alexandria)
  (:export #:start #:stop
           #:make
           #:pause
           #:resume
           #:pause-current
           #:resume-current))

(in-package :app)

(defvar *current-app* nil)

(define-condition app-event (condition)
  ())

(define-condition start (app-event)
  ())

(define-condition stop (app-event)
  ())

(define-condition pause (app-event)
  ())

(defun wait-for-start ()
  (cl-log:log-message :info "Waiting for start")
  (ccl:process-wait "Waiting for start"
                    (lambda (name)
                      (eql *current-app* name))
                    (ccl:process-name ccl:*current-process*))
  (cl-log:log-message :info "Starting"))

(defun wrap-app (function)
  (lambda ()
    (handler-bind
        ((stop (lambda (e)
                 (declare (ignore e))
                 (cl-log:log-message :info "received STOP signal")
                 (setf *current-app* nil)
                 (wait-for-start)
                 (when (find-restart 'start)
                   (invoke-restart 'start))))
         (pause (lambda (e)
                  (declare (ignore e))
                  (cl-log:log-message :info "received PAUSE signal")
                  (messaging:wait-for :code :resume)
                  (cl-log:log-message :info "received RESUME message")
                  (when (find-restart 'resume)
                    (invoke-restart 'resume)))))
      (wait-for-start)
      (funcall function))))

(defun make (name handler)
  (messaging:make-agent name (wrap-app handler)))

(defun start (app)
  (stop)
  (handler-case
      (progn
        (cl-log:log-message :info "starting app ~S" app)
        (setf *current-app* app))
    (error (e)
      (cl-log:log-message :error "Cannot start app ~S: ~A" app e)
      (stop))))

(defun stop ()
  (handler-case
      (when *current-app*
        (let ((app *current-app*))
          (cl-log:log-message :info "stopping current app ~S" app)
          (ccl:process-interrupt (messaging:agent-named app)
                                 (lambda ()
                                   (signal 'stop)))
          (ccl:process-wait (format nil "Waiting for app ~S to stop" app)
                            (lambda () (null *current-app*)))))
    (messaging:agent-not-found (e)
      (declare (ignore e))
      (setf *current-app* nil))))

(defvar *paused* nil)

(defun pause-current ()
  (when (and *current-app*
             (not *paused*))
    (setf *paused* t)
    (ccl:process-interrupt (messaging:agent-named *current-app*)
                           (lambda ()
                             (signal 'pause)))))

(defun resume-current ()
  (when *paused*
    (setf *paused* nil)
    (messaging:send *current-app* :resume)))
