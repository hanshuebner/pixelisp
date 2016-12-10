;; -*- Lisp -*-

(defpackage :server
  (:use :cl :alexandria))

(in-package :server)

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
              (erlangen:send (list :set-animation (display:load-gif gif)) :display)
            (error (e)
              (cl-log:log-message :error "Error loading gif ~A~%~A~%" gif e)
              (sleep .5))))
        (utils:sleep 30)
        (when (/= last-update (file-write-date #P"gifs/"))
          (return))))))

(defun wait-for-key (key)
  (loop
    (let ((message (erlangen:receive)))
      (cl-log:log-message :info "received message ~A" message)
      (when (equal message (list :key-pressed key))
        (return)))))

(defun start (&key (port 80))
  (logging:start)
  (storage:start)
  (display:start)
  (cl-log:log-message :info "Starting hunchentoot on port ~A" port)
  (when port
    (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                      :port port
                                      :document-root #P "html/")))
  (utils:with-agent :main
    (remote:start (erlangen:agent))
    (loop
      (utils:with-agent :player
        (play-all))
      (wait-for-key :key-power)
      (erlangen:exit 'stopped :player)
      (erlangen:send (list :blank) :display)
      (wait-for-key :key-power))))

(defun main (command-line-arguments)
  (declare (ignore command-line-arguments))
  (setf ccl:*break-hook* (lambda (cond hook)
                           (declare (ignore cond hook))
                           (format t "Exiting...~%")
                           (ccl:quit)))
  (start)
  (ccl:join-process ccl:*current-process*))
