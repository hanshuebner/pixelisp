;; -*- Lisp -*-

(defpackage :scripter
  (:use :cl :alexandria)
  (:export #:start
           #:power))

(in-package :scripter)

(defun play-all ()
  (setf *random-state* (make-random-state t))
  (loop
    (cl-log:log-message :info "Scanning GIF directory")
    (let ((gifs (directory #P"gifs/*.gif"))
          (last-update (file-write-date #P"gifs/")))
      (loop
        (let ((gif (alexandria:random-elt gifs)))
          (cl-log:log-message :info "Loading ~A" gif)
          (handler-case
              (messaging:send :display :set-animation (display:load-gif gif))
            (error (e)
              (cl-log:log-message :error "Error loading gif ~A~%~A~%" gif e)
              (sleep .5))))
        (sleep 15)
        (when (/= last-update (file-write-date #P"gifs/"))
          (return))))))

(defvar *power* t)

(defun (setf power) (power)
  (when (eq power :toggle)
    (setf power (not *power*)))
  (unless (eq power *power*)
    (ccl:process-interrupt (messaging:agent-named :scripter)
                           (lambda () (throw 'power power)))
    (setf *power* power))
  *power*)

(defun power ()
  *power*)

(defun start ()
  (messaging:make-agent :scripter
                        (lambda ()
                          (let ((power t))
                            (loop
                              (setf power (catch 'power
                                            (cond
                                              (power
                                               (cl-log:log-message :info "Power on")
                                               (loop
                                                 (cl-log:log-message :info "running animations")
                                                 (messaging:make-agent :app 'play-all)
                                                 (sleep 45)
                                                 (messaging:exit :agent-name :app)
                                                 (messaging:wait-for :code 'messaging:exit :from :app)
                                                 (cl-log:log-message :info "running clock")
                                                 (messaging:make-agent :app 'clock:run)
                                                 (sleep 15)
                                                 (messaging:exit :agent-name :app)
                                                 (messaging:wait-for :code 'messaging:exit :from :app)))
                                              (t
                                               (handler-case
                                                   (messaging:exit :agent-name :app)
                                                 (messaging:agent-not-found (e)
                                                   (declare (ignore e))))
                                               (messaging:send :display :blank)
                                               (cl-log:log-message :info "Power off")
                                               (loop (sleep 1)))))))))))
