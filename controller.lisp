;; -*- Lisp -*-

(defpackage :controller
  (:use :cl :alexandria)
  (:export #:start
           #:power))

(in-package :controller)

(defvar *power* t)

(defun (setf power) (power)
  (when (eq power :toggle)
    (setf power (not *power*)))
  (unless (eq power *power*)
    (ccl:process-interrupt (messaging:agent-named :controller)
                           (lambda () (throw 'power power)))
    (setf *power* power))
  *power*)

(defun power ()
  *power*)

(defun start ()
  (messaging:make-agent :controller
                        (lambda ()
                          (let ((power t))
                            (loop
                              (setf power (catch 'power
                                            (cond
                                              (power
                                               (cl-log:log-message :info "Power on")
                                               (loop
                                                 (cl-log:log-message :info "running animations")
                                                 (app:start :gallery)
                                                 (sleep 45)
                                                 (app:stop)
                                                 (cl-log:log-message :info "running clock")
                                                 (app:start :clock)
                                                 (sleep 15)
                                                 (app:stop)))
                                              (t
                                               (app:stop)
                                               (messaging:send :display :blank)
                                               (cl-log:log-message :info "Power off")
                                               (loop (sleep 1)))))))))))

(hunchentoot:define-easy-handler (pause :uri "/power") (switch)
  (when (eq (hunchentoot:request-method*) :post)
    (setf (power)
          (ecase (intern (string-upcase switch) :keyword)
            (:on t)
            (:off nil)
            (:toggle :toggle))))
  (if (power)
      "on"
      "off"))
