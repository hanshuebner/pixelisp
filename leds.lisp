;; -*- Lisp -*-

(defpackage :leds
  (:use :cl :alexandria)
  (:export #:start-frame-thrower
           #:current-animation
           #:load-gif
           #:name #:images
           #:send-command
           #:chill-factor
           #:brightness))

(in-package :leds)

(defvar *current-animation* nil)
(storage:defvar *brightness* 5)

(defun (setf brightness) (brightness)
  (setf *brightness* brightness))

(defun brightness ()
  *brightness*)

(storage:defvar *chill-factor* 2)

(defun (setf chill-factor) (chill-factor)
  (setf *chill-factor* chill-factor))

(defun chill-factor ()
  *chill-factor*)

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

(defun (setf buffer-brightness) (brightness buffer)
  (check-type brightness (integer 0 7) "in the expected range for brightness")
  (dotimes (x 16)
    (dotimes (y 16)
      (setf (aref buffer (* (+ (* y 16) x 1) 4)) (+ #x80 brightness)))))

(defun make-frame-buffer (&key (brightness *brightness*))
  (check-type brightness (integer 0 7) "in the expected range for brightness")
  (let ((buffer (make-array (+ 4 (* 256 4) 256) :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (buffer-brightness buffer) brightness)
    buffer))

(defun image-to-leds (color-table frame-buffer image)
  (loop for image-x from 0 below (min (skippy:width image) 16)
        do (loop for image-y from 0 below (min (skippy:height image) 16)
                 for fb-x* = (+ (skippy:left-position image) image-x)
                 for fb-y = (+ (skippy:top-position image) image-y)
                 for fb-x = (if (oddp fb-y) (- 15 fb-x*) fb-x*)
                 for fb-offset = (* (+ (* fb-y 16) fb-x 1) 4)
                 for color-index = (skippy:pixel-ref image image-x image-y)
                 when (or (not (skippy:transparency-index image))
                          (/= (skippy:transparency-index image) color-index))
                 do (multiple-value-bind (r g b) (skippy:color-rgb (skippy:color-table-entry color-table color-index))
                      (setf (aref frame-buffer (+ fb-offset 1)) b
                            (aref frame-buffer (+ fb-offset 2)) g
                            (aref frame-buffer (+ fb-offset 3)) r))))
  frame-buffer)

(defun make-led-images (file)
  (loop with stream = (skippy:load-data-stream file)
        with frame-buffer = (make-frame-buffer :brightness *brightness*)
        with images = (skippy:images stream)
        with color-table = (skippy:color-table stream)
        for i from 0 below (length images)
        for image = (aref images i)
        do (setf frame-buffer (image-to-leds color-table frame-buffer image)
                 (aref images i) (change-class image 'led-image
                                               :led-frame (copy-sequence 'vector frame-buffer)))
        finally (return images)))

(defun load-gif (file)
  (cl-log:log-message :info "Loading GIF file ~A" file)
  (let* ((images (make-led-images file)))
    (cl-log:log-message :info "Loaded ~D frames" (length images))
    (make-instance 'animation
                   :name (pathname-name file)
                   :images images)))

(defun display-frame (output image)
  (let ((start-time (get-internal-real-time)))
    (write-sequence (led-frame image) output)
    (let* ((write-duration (/ (- (get-internal-real-time) start-time) internal-time-units-per-second))
           (delay (- (* (/ (if (zerop (skippy:delay-time image))
                               10
                               (skippy:delay-time image))
                           100)
                        *chill-factor*)
                     write-duration)))
      (when (plusp delay)
        (sleep delay)))))

(defun set-current-animation (animation)
  (setf (next-image-index animation) 0
        *current-animation* animation)
  (events:publish :animation-loaded (name animation)))

(defun blank (output)
  (setf *current-animation* nil)
  (write-sequence (make-frame-buffer :brightness 0) output))

(defparameter *leds-device* "/dev/spidev0.0")

(defun display-loop (queue)
  (let ((output (open *leds-device* :direction :output
                                    :if-exists :append
                                    :element-type '(unsigned-byte 8)))
        (brightness *brightness*))
    (loop
      (when-let (command (queues:qpop queue))
        (ecase (first command)
          (:quit
           (return))
          (:set-animation
           (set-current-animation (second command)))
          (:blank
           (blank output))))
      (cond
        (*current-animation*
         (unless (= brightness *brightness*)
           ;; Execute brightness change, if any.
           (setf brightness *brightness*)
           (loop for image across (images *current-animation*)
                 do (setf (buffer-brightness (led-frame image)) brightness)))
         (display-frame output (next-image *current-animation*)))
        (t
         (sleep .01))))))

(defvar *frame-thrower-thread* nil)

(defvar *commands-queue*)

(defun start-frame-thrower ()
  (when (and *frame-thrower-thread*
             (bt:thread-alive-p *frame-thrower-thread*))
    (error "frame thrower already running"))
  (cl-log:log-message :info "Starting frame-thrower thread")
  (let ((queue (queues:make-queue :simple-cqueue)))
    (setf *frame-thrower-thread* (bt:make-thread (lambda ()
                                                   (handler-case
                                                       (display-loop queue)
                                                     (error (e)
                                                       (cl-log:log-message :error "frame-thrower thread died with error ~A" e)))))
          *commands-queue* queue)))

(defun send-command (command &rest args)
  (queues:qpush *commands-queue* `(,command ,@args)))
