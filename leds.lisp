;; -*- Lisp -*-

(defpackage :leds
  (:use :cl :alexandria)
  (:export #:start-frame-thrower
           #:current-animation
           #:load-gif
           #:name #:images))

(in-package :leds)

(defvar *current-animation* nil)

(defun current-animation ()
  *current-animation*)

(defclass animation ()
  ((images :initarg :images :reader images)
   (name :initarg :name :reader name)
   (next-image-index :initform 0 :accessor next-image-index)))

(defmethod print-object ((animation animation) stream)
  (print-unreadable-object (animation stream :type t)
    (format stream "NAME: ~A (~:D frames, at ~D)"
            (name animation)
            (length (images animation))
            (next-image-index animation))))

(defmethod next-image ((animation animation))
  (with-slots (images next-image-index) animation
    (prog1
        (aref images next-image-index)
      (incf next-image-index)
      (when (= next-image-index (length images))
        (setf next-image-index 0)))))

(defclass led-image (skippy::image)
  ((led-frame :initarg :led-frame :reader led-frame)))

(defun get-rgb (color-table image x y)
  (multiple-value-list (skippy:color-rgb (skippy:color-table-entry color-table
                                                                   (skippy:pixel-ref image x y)))))

(defun image-to-leds (color-table image)
  (flexi-streams:with-output-to-sequence (stream)
    (write-sequence #(0 0 0 0) stream)
    (dotimes (x 16)
      (dotimes (y 16)
        (write-sequence #(#x87) stream)
        (write-sequence (get-rgb color-table image x y) stream)))
    (write-sequence (make-array (/ 256 2 8) :element-type '(unsigned-byte 8) :initial-element 0)
                    stream)))

(defun load-gif (file)
  (cl-log:log-message :info "Loading GIF file ~A" file)
  (let* ((stream (skippy:load-data-stream file))
         (images (map 'vector
                      (lambda (image)
                        (change-class image 'led-image
                                      :led-frame (image-to-leds (skippy:color-table stream) image)))
                      (skippy:images stream))))
    (cl-log:log-message :info "Loaded ~D frames" (length images))
    (make-instance 'animation
                   :name (pathname-name file)
                   :images images)))

(defun display-frame (output image)
  (let ((start-time (get-internal-real-time)))
    (write-sequence (led-frame image) output)
    (let* ((write-duration (/ (- (get-internal-real-time) start-time) internal-time-units-per-second))
           (delay (- (/ (skippy:delay-time image) 100) write-duration)))
      (sleep delay))))

(defun set-current-animation (animation)
  (setf (next-image-index animation) 0
        *current-animation* animation)
  (events:publish :animation-loaded (name animation)))

(defparameter *leds-device* "/dev/spidev0.0")

(defun display-loop (command-queue)
  (let ((output (open *leds-device* :direction :output
                                    :if-exists :append
                                    :element-type '(unsigned-byte 8))))
    (loop
      (when-let (command (queues:qpop command-queue))
        (ecase (first command)
          (:quit
           (return))
          (:set-animation
           (set-current-animation (second command)))))
      (cond
        (*current-animation*
         (display-frame output (next-image *current-animation*)))
        (t
         (sleep .01))))))

(defvar *frame-thrower-thread* nil)

(defun start-frame-thrower (command-queue)
  (when (and *frame-thrower-thread*
             (bt:thread-alive-p *frame-thrower-thread*))
    (error "frame thrower already running"))
  (cl-log:log-message :info "Starting frame-thrower thread")
  (setf *frame-thrower-thread*
        (bt:make-thread (lambda ()
                          (handler-case
                              (display-loop command-queue)
                            (error (e)
                              (cl-log:log-message :error "frame-thrower thread died with error ~A" e)))))))
