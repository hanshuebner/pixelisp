;; -*- Lisp -*-

(defpackage :alerter
  (:use :cl :alexandria)
  (:export #:start))

(in-package :alerter)

(hunchentoot:define-easy-handler (alert-set :uri "/alert/set") (message color loop)
  (unless (eq (hunchentoot:request-method*) :post)
    (error "need POST request"))
  (messaging:send :alerter :set
                  :text message
                  :color (color:parse color)
                  :loop (when loop
                          (or (parse-integer loop :junk-allowed t)
                              (and (string-equal loop "forever") t)
                              (error "invalid loop specification, need number or 'forever'"))))
  "alert set")

(hunchentoot:define-easy-handler (alert-cancel :uri "/alert/cancel") (abort)
  (unless (eq (hunchentoot:request-method*) :post)
    (error "need POST request"))
  (messaging:send :alerter
                  (if (and abort
                           (not (equal abort "0")))
                      :abort
                      :cancel))
  "alert ended")

(defun resume-running-app ()
  (messaging:send :display :blank))

(defun start ()
  (messaging:make-agent :alerter
                        (lambda ()
                          (let (current-loop)
                            (loop
                              (let ((message (messaging:receive)))
                                (ecase (messaging:code message)
                                  (:set
                                   (destructuring-bind (&key text color loop) (messaging:args message)
                                     (setf current-loop loop)
                                     (messaging:send :display
                                                     :set-animation (display:make-text text
                                                                                       :color color
                                                                                       :owner (ccl:process-name (messaging:agent))))))
                                  (:cancel
                                   (setf current-loop nil))
                                  (:abort
                                   (setf current-loop nil)
                                   (resume-running-app))
                                  (:animation-at-start
                                   (when (integerp current-loop)
                                     (decf current-loop)
                                     (unless (plusp current-loop)
                                       (setf current-loop nil)))
                                   (unless current-loop
                                     (resume-running-app))))))))))
