;; -*- Lisp -*-

(defpackage :utils
  (:use :cl :alexandria)
  (:export #:run-program))

(in-package :utils)

(defun run-program (program &rest args)
  (with-output-to-string (output)
    (multiple-value-bind (status exit-code)
        (ccl:external-process-status (ccl:run-program program args
                                                      :wait t
                                                      :output output))
      (declare (ignore status))
      (unless (zerop exit-code)
        (error 'simple-error
               :format-control "shell command \"~A~@[ ~A~]\" failed with exit code ~D~@[~%~A~]"
               :format-arguments (list program args exit-code (get-output-stream-string output)))))))
