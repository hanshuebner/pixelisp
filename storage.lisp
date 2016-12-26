;; -*- Lisp -*-

(defpackage :storage
  (:use :cl :alexandria)
  (:export #:defconfig
           #:config
           #:start))

(in-package :storage)

(defvar *config* (make-hash-table))

(defun from-storage (variable default)
  (gethash variable *config* default))

(defun register-persistent-variable (variable initial-value)
  (setf (gethash variable *config*) initial-value))

(defun defconfig (variable initial-value)
  (register-persistent-variable variable initial-value))

(defun key-valid-p (key)
  (nth-value 1 (gethash key *config*)))

(defun validate-key (key)
  (or (key-valid-p key)
      (error "invalid configuration key ~S" key)))

(defun config (variable)
  (validate-key variable)
  (gethash variable *config* 'not-found))

(defun (setf config) (value variable)
  (validate-key variable)
  (setf (gethash variable *config*) value)
  (messaging:send :store-snapshot :flush))

(defparameter *state-file* #P"settings.dat")

(defun save ()
  (with-open-file (output *state-file* :direction :output
                                       :if-exists :new-version
                                       :element-type '(unsigned-byte 8))
    (write-sequence (flex:with-output-to-sequence (s)
                      (cl-store:store *config* s))
                    output))
  (cl-log:log-message :info "Saved settings to ~A" (truename *state-file*)))

(defmethod cl-store:backend-restore :around ((store cl-store:cl-store) (input t))
  (handler-bind
      ((ccl::no-such-package (lambda (e)
                               (invoke-restart 'use-value (make-package (slot-value e 'package))))))
    (call-next-method)))

(defun read-storage (file)
  (flex:with-input-from-sequence (s (alexandria:read-file-into-byte-vector file))
    (cl-store:restore s)))

(defun restore ()
  (when (probe-file *state-file*)
    (let ((restored-config (read-storage *state-file*)))
      (maphash (lambda (key value)
                 (if (key-valid-p key)
                     (setf (gethash key *config*) value)
                     (cl-log:log-message :warning "ignoring invalid configuration key ~S read from storage" key)))
               restored-config))
    (cl-log:log-message :info "Restored settings from ~A" (truename *state-file*))))

(defparameter *snapshot-interval* 5)

(defun start ()
  (restore)
  (messaging:make-agent :store-snapshot
                        (lambda ()
                          (loop
                            (sleep *snapshot-interval*)
                            (when (messaging:try-receive)
                              (loop
                                (unless (messaging:try-receive)
                                  (return)))
                              (save))))))
