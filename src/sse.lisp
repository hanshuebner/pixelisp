;; -*- Lisp -*-

(defpackage :sse
  (:use :cl :alexandria)
  (:export #:event
           #:idle-event
           #:data
           #:id
           #:retry
           #:send-event
           #:client))

(in-package :sse)

(defclass event ()
  ((event :initform nil :initarg :event :reader event)
   (data :initform nil :initarg :data :reader data)
   (id :initform nil :initarg :id :reader id)
   (retry :initform nil :initarg :retry :reader retry)))

(defmethod print-object ((event event) stream)
  (print-unreadable-object (event stream :type t)
    (format stream "EVENT: ~A DATA: ~S" (event event) (data event))))

(defmethod event-string ((event event))
  (format nil
          "~@[event: ~(~A~)~%~]~
           ~:[:~;~:*data: ~A~]~%~
           ~@[id: ~A~%~]~
           ~@[retry: ~A~%~]~
           ~%"
          (event event)
          (data event)
          (id event)
          (retry event)))

(defclass idle-event ()
  ())

(defmethod print-object ((event idle-event) stream)
  (print-unreadable-object (event stream :type t)))

(defmethod event-string ((event idle-event))
  (format nil ": idle~%~%"))

(defvar *client-id* 0)

(defclass client ()
  ((stream :initarg :stream :reader stream)
   (lock :initform (ccl:make-lock "event-stream-lock") :accessor lock)
   (id :initform (ccl::atomic-incf *client-id*) :reader id)))

(defmethod print-object ((client client) stream)
  (print-unreadable-object (client stream :type t)
    (format stream "ID: ~A" (id client))))

(defmethod send-event ((client client) event)
  (ccl:with-lock-grabbed ((lock client))
    (write-string (event-string event) (stream client))
    (finish-output (stream client))))
