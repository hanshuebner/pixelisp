;; -*- Lisp -*-

(defpackage :leds
  (:use :cl :alexandria)
  (:export #:start-frame-thrower
           #:current-animation
           #:load-gif
           #:name #:images))

(in-package :leds)

(defvar *current-animation* nil)
(defvar *brightness* 7)

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

(defun make-frame-buffer (&key (brightness *brightness*))
  (check-type brightness (integer 0 7) "in the expected range for brightness")
  (let ((buffer (make-array (+ 4 (* 256 4) 256) :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (x 16)
      (dotimes (y 16)
        (setf (aref buffer (* (+ (* y 16) x 1) 4)) (+ #x80 brightness))))
    buffer))

(defun image-to-leds (color-table frame-buffer image x-scale y-scale)
  (loop for image-x from 0 below (skippy:width image) by x-scale
        do (loop for image-y from 0 below (skippy:height image) by y-scale
                 for fb-x* = (floor (+ (skippy:left-position image) image-x) x-scale)
                 for fb-y = (floor (+ (skippy:top-position image) image-y) y-scale)
                 for fb-x = (if (oddp fb-y) (- 15 fb-x*) fb-x*)
                 for fb-offset = (* (+ (* fb-y 16) fb-x 1) 4)
                 for color-index = (skippy:pixel-ref image image-x image-y)
                 when (/= (skippy:transparency-index image) color-index)
                 do (multiple-value-bind (r g b) (skippy:color-rgb (skippy:color-table-entry color-table color-index))
                      (setf (aref frame-buffer (+ fb-offset 1)) b
                            (aref frame-buffer (+ fb-offset 2)) g
                            (aref frame-buffer (+ fb-offset 3)) r))))
  frame-buffer)

(defun make-led-images (stream)
  (loop with frame-buffer = (make-frame-buffer :brightness *brightness*)
        with original-images = (skippy:images stream)
        with images = (make-array (length original-images))
        with x-scale = (/ (skippy:width stream) 16)
        with y-scale = (/ (skippy:height stream) 16)
        with color-table = (skippy:color-table stream)
        for i from 0 below (length original-images)
        for image = (aref original-images i)
        do (setf frame-buffer (image-to-leds color-table frame-buffer (aref original-images i) x-scale y-scale)
                 (aref images i) (change-class image 'led-image
                                               :led-frame (copy-sequence 'vector frame-buffer)))
        finally (return images)))

(defun load-gif (file)
  (cl-log:log-message :info "Loading GIF file ~A" file)
  (let* ((images (make-led-images (skippy:load-data-stream file))))
    (cl-log:log-message :info "Loaded ~D frames" (length images))
    (make-instance 'animation
                   :name (pathname-name file)
                   :images images)))

(defun display-frame (output image)
  (let ((start-time (get-internal-real-time)))
    (write-sequence (led-frame image) output)
    (let* ((write-duration (/ (- (get-internal-real-time) start-time) internal-time-units-per-second))
           (delay (- (/ (skippy:delay-time image) 100) write-duration)))
      (when (plusp delay)
        (sleep delay)))))

(defun set-current-animation (animation)
  (setf (next-image-index animation) 0
        *current-animation* animation)
  (events:publish :animation-loaded (name animation)))

(defun stop (output)
  (setf *current-animation* nil)
  (write-sequence (make-frame-buffer :brightness 0) output))

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
           (set-current-animation (second command)))
          (:stop
           (stop output))))
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
