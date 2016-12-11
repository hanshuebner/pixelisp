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
              (messaging:send :display :set-animation (display:load-gif gif))
            (error (e)
              (cl-log:log-message :error "Error loading gif ~A~%~A~%" gif e)
              (sleep .5))))
        (sleep 30)
        (when (/= last-update (file-write-date #P"gifs/"))
          (return))))))

(defun start (&key (port 80))
  (logging:start)
  (storage:start)
  (display:start)
  (cl-log:log-message :info "Starting hunchentoot on port ~A" port)
  (when port
    (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                      :port port
                                      :document-root #P "html/")))
  (messaging:make-agent :main
                        (lambda ()
                          (remote-control:start :main)
                          (loop
                            (messaging:make-agent :app 'play-all
                                                  :parent ccl:*current-process*)
                            (messaging:wait-for '(:key-pressed :key-power) :from :remote-control-reader)
                            (messaging:exit :agent-name :app)
                            (messaging:wait-for '(:exit) :from :app)
                            (messaging:send :display :blank)
                            (messaging:wait-for '(:key-pressed :key-power) :from :remote-control-reader)))))

(defun main (command-line-arguments)
  (declare (ignore command-line-arguments))
  (setf ccl:*break-hook* (lambda (cond hook)
                           (declare (ignore cond hook))
                           (format t "Exiting...~%")
                           (ccl:quit)))
  (start)
  (ccl:join-process ccl:*current-process*))
