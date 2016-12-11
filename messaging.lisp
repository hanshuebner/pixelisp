;; -*- Lisp -*-

(defpackage :messaging
  (:use :cl :alexandria)
  (:shadow #:sleep)
  (:export #:try-receive
           #:sleep
           #:send
           #:agent-name
           #:receive
           #:wait-for
           #:exit
           #:condition
           #:ok
           #:code
           #:from
           #:args
           #:kill
           #:kill-all
           #:make-agent))

(in-package :messaging)

(defvar *agent-registry* (make-hash-table :test #'equal))

(defclass agent (ccl:process)
  ((mailbox :initform (safe-queue:make-mailbox)
            :reader mailbox)
   (parent :initarg :parent
           :initform nil
           :reader parent)))

(defmethod print-object ((agent agent) stream)
  (print-unreadable-object (agent stream :type t)
    (format stream "~A" (ccl:process-name agent))))

(define-condition duplicate-process-name (simple-error)
  ((name :initarg :name
         :reader name))
  (:default-initargs :format-control "Duplicate process name: ~A"))

(defmethod initialize-instance :before ((agent agent) &key name)
  ;; FIXME: race condition if two processes with the same name are generated at the sime time
  (when (gethash name *agent-registry*)
    (error 'duplicate-process-name :name name :format-arguments (list name)))
  (setf (gethash name *agent-registry*) agent))

(defclass message ()
  ((from :initform ccl:*current-process*
         :reader from)
   (code :initarg :code
         :initform (error "missing :code argument to make-instance of message")
         :reader code)
   (args :initarg :args
         :initform nil
         :reader args)))

(defmethod print-object ((message message) stream)
  (print-unreadable-object (message stream :type t)
    (format stream "~S~@[ ~S~]" (code message) (args message))))

(defun maybe-notify-parent (reason data)
  (when (and (typep ccl:*current-process* 'agent)
             (parent ccl:*current-process*))
    (safe-queue:enqueue (make-instance :code 'exit
                                       :args (list reason data))
                         (mailbox (parent ccl:*current-process*)))))

(defun run-agent (function)
  (lambda ()
    (unwind-protect
         (handler-case
             (handler-bind
                 ((error (lambda (e)
                           (format t "~A~%~{~A~%~}" e (ccl:backtrace-as-list))))) (cl-log:log-message :info "agent ~A starting" (ccl:process-name ccl:*current-process*))
               (catch 'exit
                 (funcall function))
               (maybe-notify-parent 'ok nil)
               (cl-log:log-message :info "agent ~A exiting normally" (ccl:process-name ccl:*current-process*)))
           (condition (e)
             (cl-log:log-message :info "agent ~A exiting with error: ~A" (ccl:process-name ccl:*current-process*) e)
             (maybe-notify-parent 'condition e)))
      (remhash (ccl:process-name ccl:*current-process*) *agent-registry*))))

(defun make-agent (name function &key (class 'agent) parent)
  (ccl:process-run-function (list :name name
                                  :class class
                                  :initargs (list :parent parent))
                            (run-agent function)))

(define-condition agent-not-found (simple-error)
  ((name :initarg :name :reader name))
  (:default-initargs
   :format-control "No agent named ~S was found"))

(defun agent-named (name)
  (or (gethash name *agent-registry*)
      (error 'agent-not-found :name name :format-arguments (list name))))

(defun send (to code &rest args)
  (safe-queue:mailbox-send-message (mailbox (agent-named to))
                                   (make-instance 'message :code code :args args)))

(defun receive (&key timeout)
  (safe-queue:mailbox-receive-message (mailbox ccl:*current-process*)
                                      :timeout timeout))

(defun try-receive ()
  (safe-queue:mailbox-receive-message-no-hang (mailbox ccl:*current-process*)))

(defun wait-for (message &key from)
  (cl-log:log-message :info "waiting for ~S ~@[ from ~S~]" message from)
  (destructuring-bind (expected-code &rest expected-args) message
    (loop
      (let* ((received-message (receive))
             (received-from (ccl:process-name (from received-message))))
        (cl-log:log-message :info "received message ~S from ~S" received-message received-from)
        (when (and (eql (or from received-from)
                        received-from)
                   (eql (code received-message) expected-code)
                   (equal (args received-message) expected-args))
          (return))))))

(defun exit (&key reason agent-name)
  (if agent-name
      (ccl:process-interrupt (agent-named agent-name)
                             (lambda ()
                               (throw 'exit reason)))
      (throw 'exit reason)))

(defun kill (agent-name)
  (ccl:process-interrupt (agent-named agent-name)
                         (lambda ()
                           (throw 'exit 'killed))))

(defun kill-all ()
  (dolist (process (ccl:all-processes))
    (when (typep process 'agent)
      (kill (ccl:process-name process)))))
