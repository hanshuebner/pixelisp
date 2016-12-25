;; -*- Lisp -*-

(defpackage :gallery
  (:use :cl :alexandria)
  (:export #:play
           #:images))

(in-package :gallery)

(storage:defconfig 'playlist nil)

(defun play ()
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

(defun images ()
  (storage:config 'playlist))

(defun (setf images) (new)
  (setf (storage:config 'playlist) new)
  new)
