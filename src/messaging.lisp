;; -*- Lisp -*-

(defpackage :messaging
  (:use :cl :alexandria)
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
           #:make-agent
           #:agent
           #:agent-named
           #:agent-not-found))

(in-package :messaging)

(defvar *agent-registry* (make-hash-table :test #'equal))

(defvar *registry-lock* (ccl:make-lock "agent registry lock"))

(define-condition duplicate-process-name (simple-error)
  ((name :initarg :name
         :reader name))
  (:default-initargs :format-control "Duplicate agent name: ~S"))

(defun register-agent (agent name)
  (ccl:with-lock-grabbed (*registry-lock*)
    (when (gethash name *agent-registry*)
      (error 'duplicate-process-name :name name :format-arguments (list name)))
    (setf (gethash name *agent-registry*) agent)))

(define-condition agent-not-found (simple-error)
  ((name :initarg :name :reader name))
  (:default-initargs
   :format-control "No agent named ~S was found"))

(defun agent-named (name)
  (or (gethash name *agent-registry*)
      (error 'agent-not-found :name name :format-arguments (list name))))

(define-condition not-an-agent (simple-error)
  ((process :initarg :process :reader process))
  (:default-initargs
   :format-control "The process ~A is not an agent"))

(defun ensure-agent (thing)
  (cond
    ((typep thing 'ccl:process)
     (if (typep thing 'agent)
         thing
         (error 'not-an-agent :process thing :format-arguments (list thing))))
    ((typep thing '(or string symbol))
     (agent-named thing))
    (t
     (error "Cannot resolve ~S (which has type ~A) to an agent"
            thing (type-of thing)))))

(defclass agent (ccl:process)
  ((mailbox :initform (safe-queue:make-mailbox)
            :reader mailbox)
   (parent :initarg :parent
           :initform nil
           :reader parent)))

(defmethod print-object ((agent agent) stream)
  (print-unreadable-object (agent stream :type t)
    (format stream "~A" (handler-case
                            (ccl:process-name agent)
                          (unbound-slot (e)
                            (declare (ignore e))
                            "[unknown name]")))))

(defun agent ()
  (unless (typep ccl:*current-process* 'agent)
    (error 'not-an-agent :process ccl:*current-process*))
  ccl:*current-process*)

(defmethod initialize-instance :before ((agent agent) &key name)
  (register-agent agent name))

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
    (format stream "from: ~S: ~S~@[ ~S~]" (from message) (code message) (args message))))

(defun handle-agent-exit (reason data)
  (remhash (ccl:process-name ccl:*current-process*) *agent-registry*)
  (when (and (typep ccl:*current-process* 'agent)
             (parent ccl:*current-process*))
    (safe-queue:enqueue (make-instance 'message
                                       :code 'exit
                                       :args (list reason data))
                        (mailbox (parent ccl:*current-process*)))))

(defun run-agent (start-semaphore function)
  (lambda ()
    (handler-case
        (handler-bind
            ((error (lambda (e)
                      (cl-log:log-message :error "~A~%~{~A~%~}" e (ccl:backtrace-as-list)))))
          (cl-log:log-message :info "agent ~S starting" (ccl:process-name ccl:*current-process*))
          (handle-agent-exit 'exit
                             (catch 'exit
                               (ccl:signal-semaphore start-semaphore)
                               (funcall function)))
          (cl-log:log-message :info "agent ~S exiting normally" (ccl:process-name ccl:*current-process*)))
      (serious-condition (e)
        (cl-log:log-message :info "agent ~S exiting with error: ~A" (ccl:process-name ccl:*current-process*) e)
        (handle-agent-exit 'condition e)))))

(defun make-agent (name function &key (class 'agent) (parent (messaging:agent)))
  (when (and parent
             (not (typep parent 'agent)))
    (error ":PARENT must be an AGENT, but it is a ~S" (type-of parent)))
  (let ((start-semaphore (ccl:make-semaphore)))
    (ccl:process-run-function (list :name name
                                    :class class
                                    :initargs (list :parent parent))
                              (run-agent start-semaphore function))
    (unless (ccl:timed-wait-on-semaphore start-semaphore 1)
      (error "agent process did not signal coordination semaphore within one second, start failed"))))

(defun send (to code &rest args)
  (safe-queue:mailbox-send-message (mailbox (agent-named to))
                                   (make-instance 'message :code code :args args)))

(defun receive (&key timeout)
  (safe-queue:mailbox-receive-message (mailbox ccl:*current-process*)
                                      :timeout timeout))

(defun try-receive ()
  (safe-queue:mailbox-receive-message-no-hang (mailbox ccl:*current-process*)))

(defun wait-for (&key from code)
  (cl-log:log-message :info "waiting for~:[ any message~; ~:*~S~]~@[ from ~S~]" code from)
  (let ((from (and from (ensure-agent from))))
    (loop
      (let* ((received-message (receive))
             (received-from (from received-message))
             (received-code (code received-message)))
        (cl-log:log-message :info "received message ~S" received-message)
        (when (and (or (null from)
                       (eql from received-from))
                   (or (null code)
                       (eql code received-code)))
          (return received-message))))))

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

(defun agentp (thing)
  (typep thing 'agent))

(defun kill-all ()
  (dolist (process (remove-if-not #'agentp (ccl:all-processes)))
    (kill (ccl:process-name process))))

