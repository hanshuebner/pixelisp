;; -*- Lisp -*-

(defpackage :leds
  (:use :cl :alexandria)
  (:export #:make-frame-thrower))

(in-package :leds)

(defclass led-image (skippy::image)
  ((led-frame :initarg :led-frame :reader led-frame)))

(defun image-to-leds (color-table image)
  (flexi-streams:with-output-to-sequence (stream)
    (write-sequence #(0 0 0 0) stream)
    (dotimes (x 16)
      (dotimes (y 16)
        (write-sequence (multiple-value-list (skippy:color-rgb (skippy:color-table-entry color-table
                                                                                         (skippy:pixel-ref image x y))))
                        stream)))
    (write-sequence (make-array (/ 256 2 8) :element-type '(unsigned-byte 8) :initial-element 0)
                    stream)))

(defun load-gif (file)
  (let ((stream (skippy:load-data-stream file)))
    (map 'list
         (lambda (image)
           (change-class image 'led-image
                         :led-frame (image-to-leds (skippy:color-table stream) image)))
         (skippy:images stream))))

(defun display-frame (output image)
  (let ((start-time (get-internal-real-time)))
    (write-sequence (led-frame image) output)
    (let* ((write-duration (/ (- (get-internal-real-time) start-time) internal-time-units-per-second))
           (delay (- (/ (skippy:delay-time image) 100) write-duration)))
      (sleep delay))))

(defparameter *leds-device* "/dev/null")

(defun display-loop (command-queue)
  (let (images
        (output (open *leds-device* :direction :output
                                    :if-exists :append
                                    :element-type '(unsigned-byte 8))))
    (loop
      (when-let (command (queues:qpop command-queue))
        (ecase (first command)
          (:quit
           (return))
          (:load-gif
           (setf images (load-gif (second command))))))
      (cond
        (images
         (display-frame output (first images))
         (setf images (append (rest images)
                              (list (first images)))))
        (t
         (sleep .01))))))

(defvar *frame-thrower-thread* nil)

(defun make-frame-thrower (command-queue)
  (when (and *frame-thrower-thread*
             (bt:thread-alive-p *frame-thrower-thread*))
    (error "frame thrower already running"))
  (setf *frame-thrower-thread*
        (bt:make-thread (lambda ()
                          (handler-case
                              (display-loop command-queue)
                            (error (e)
                              (format t "frame thrower thread died because of error: ~A~%" e)))))))
