;; -*- Lisp -*-

(defpackage :storage
  (:use :cl :alexandria)
  (:shadow cl:defvar)
  (:export #:defvar
           #:start))

;;; Note: this storage mechanism must only be used for immutable
;;; variable values, as it does not provide a thread safe way to read
;;; variable states while saving.

(in-package :storage)

(cl:defvar *persistent-variables* (make-hash-table))

(defun from-storage (variable)
  (gethash variable *persistent-variables*))

(defun register-persistent-variable (variable initial-value)
  (setf (gethash variable *persistent-variables*) initial-value))

(defmacro defvar (variable initial-value &optional docstring)
  `(progn
     (cl:defvar ,variable (load-time-value (from-storage ',variable)) ,docstring)
     (register-persistent-variable ',variable ,initial-value)))

(cl:defvar *last-stored-state* nil)
(defparameter *state-file* #P"settings.dat")

(defun maybe-save ()
  (dolist (variable (hash-table-keys *persistent-variables*))
    (setf (gethash variable *persistent-variables*) (symbol-value variable)))
  (let ((stored-state (flex:with-output-to-sequence (s)
                        (cl-store:store *persistent-variables* s))))
    (unless (equalp stored-state *last-stored-state*)
      (setf *last-stored-state* stored-state)
      (with-open-file (output *state-file* :direction :output
                                           :if-exists :new-version
                                           :element-type '(unsigned-byte 8))
        (write-sequence stored-state output))
      (cl-log:log-message :info "Saved settings to ~A" (truename *state-file*)))))

(defun restore ()
  (when (probe-file *state-file*)
    (setf *last-stored-state* (alexandria:read-file-into-byte-vector *state-file*)
          *persistent-variables* (flex:with-input-from-sequence (s *last-stored-state*)
                                   (cl-store:restore s)))
    (dolist (variable (hash-table-keys *persistent-variables*))
      (setf (symbol-value variable) (gethash variable *persistent-variables*)))
    (cl-log:log-message :info "Restored settings from ~A" (truename *state-file*))))

(defparameter *snapshot-interval* 5)

(defun start ()
  (restore)
  (ccl:process-run-function "store-snapshot"
                            (lambda ()
                              (loop
                                (sleep *snapshot-interval*)
                                (maybe-save)))))
