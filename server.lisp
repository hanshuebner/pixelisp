;; -*- Lisp -*-

(defpackage :server
  (:use :cl :alexandria))

(in-package :server)

(push (hunchentoot:create-folder-dispatcher-and-handler "/gif/" #P"gifs/") hunchentoot:*dispatch-table*)

(defun start (&key (port 8428))
  (let ((led-commands (queues:make-queue :simple-cqueue)))
    (leds:start-frame-thrower led-commands)
    (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                      :port port
                                      :document-root #P "html/"))))
