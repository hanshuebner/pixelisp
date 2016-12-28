;; -*- Lisp -*-

(defpackage :web-frame
  (:use :cl :alexandria)
  (:export #:start))

(in-package :web-frame)

(defvar *last-frame-event* nil)

(hunchentoot:define-easy-handler (events :uri "/events") ()
  (setf (hunchentoot:content-type*) "text/event-stream")
  (let ((client (make-instance 'sse:client
                               :stream (flexi-streams:make-flexi-stream (hunchentoot:send-headers))))
        (handler-process ccl:*current-process*))
    (cl-log:log-message :info "Event client ~A connected" client)
    (catch 'disconnect
      (when *last-frame-event*
        (handler-bind
            (((or ccl:socket-error stream-error) (lambda (e)
                                                   (throw 'disconnect e))))
          (sse:send-event client *last-frame-event*)))
      (events:subscribe (lambda (event)
                          (handler-bind
                              (((or ccl:socket-error stream-error) (lambda (e)
                                                                     (ccl:process-interrupt handler-process (lambda ()
                                                                                                              (throw 'disconnect e))))))
                            (sse:send-event client (make-instance 'sse:event
                                                                  :event (string-downcase (events:type event))
                                                                  :data (events:data event)
                                                                  :id (events:id event))))))
      (handler-case
          (loop
            (sse:send-event client (make-instance 'sse:idle-event))
            (sleep 10))
        (ccl:socket-error (e)
          (declare (ignore e)))))
    (cl-log:log-message :info "Event client ~A disconnected" client)))

(defun start ()
  (events:subscribe (lambda (event)
                      (when (eql (events:type event) :frame)
                        (setf *last-frame-event* (make-instance 'sse:event
                                                                :event (string-downcase (events:type event))
                                                                :data (events:data event)
                                                                :id (events:id event)))))))
