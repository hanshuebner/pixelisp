;; -*- Lisp -*-

(defpackage :utils
  (:use :cl :alexandria)
  (:export #:run-program
           #:settings-hash))

(in-package :utils)

(defun run-program (program &rest args)
  (with-output-to-string (output)
    (multiple-value-bind (status exit-code)
        (ccl:external-process-status (ccl:run-program program args
                                                      :wait t
                                                      :output output))
      (declare (ignore status))
      (unless (zerop exit-code)
        (error "shell command \"~A~@[ ~A~]\" failed with exit code ~D~@[~%~A~]"
               program args exit-code (get-output-stream-string output))))))

(defun settings-hash (&rest args)
  (loop with hash = (make-hash-table :test #'equal)
        for (k v) on args by #'cddr
        do (setf (gethash (string-downcase k) hash) v)
        finally (return hash)))
