;; -*- Lisp -*-

(defpackage :display
  (:use :cl :alexandria)
  (:export #:start
           #:current-animation
           #:load-gif
           #:name #:images
           #:brightness
           #:chill-factor
           #:image-to-leds))

(in-package :display)

(defparameter *leds-device* #-darwin "/dev/spidev0.0" #+darwin "/dev/null")

(defvar *current-animation* nil)

(storage:defconfig 'brightness 5)
(storage:defconfig 'chill-factor 2)

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

(defun make-frame-buffer (&key (brightness (storage:config 'brightness)))
  (check-type brightness (integer 0 7) "in the expected range for brightness")
  (let ((buffer (make-array (+ 4 (* 256 4) 256) :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (buffer-brightness buffer) brightness)
    buffer))

(defun image-to-leds (image color-table &key (frame-buffer (make-frame-buffer)))
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
        with frame-buffer = (make-frame-buffer :brightness (storage:config 'brightness))
        with images = (skippy:images stream)
        with color-table = (skippy:color-table stream)
        for i from 0 below (length images)
        for image = (aref images i)
        do (setf frame-buffer (image-to-leds image color-table :frame-buffer frame-buffer)
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

(defun write-frame (frame output)
  (write-sequence frame output)
  (events:publish "frame" (time (format nil "~{~2,'0X~}" (coerce frame 'list)))))

(defun display-frame (output image)
  (let ((start-time (get-internal-real-time)))
    (write-frame (led-frame image) output)
    (let* ((write-duration (/ (- (get-internal-real-time) start-time) internal-time-units-per-second))
           (delay (- (* (/ (if (zerop (skippy:delay-time image))
                               10
                               (skippy:delay-time image))
                           100)
                        (storage:config 'chill-factor))
                     write-duration)))
      (when (plusp delay)
        (sleep delay)))))

(defun set-current-animation (animation)
  (setf (next-image-index animation) 0
        *current-animation* animation)
  (events:publish :animation-loaded (name animation)))

(defun set-current-frame-buffer (output frame-buffer)
  (setf *current-animation* nil)
  (write-frame frame-buffer output))

(defun blank (output)
  (setf *current-animation* nil)
  (write-frame (make-frame-buffer :brightness 0) output))

(defun display-loop ()
  (let ((output (open *leds-device* :direction :output
                                    :if-exists :append
                                    :element-type '(unsigned-byte 8)))
        (brightness (storage:config 'brightness)))
    (blank output)
    (loop
      (when-let (message (messaging:try-receive))
        (ecase (messaging:code message)
          (:quit
           (return))
          (:set-animation
           (apply #'set-current-animation (messaging:args message)))
          (:set-frame-buffer
           (apply #'set-current-frame-buffer output (messaging:args message)))
          (:blank
           (blank output))))
      (cond
        (*current-animation*
         (unless (= brightness (storage:config 'brightness))
           ;; Execute brightness change, if any.
           (setf brightness (storage:config 'brightness))
           (loop for image across (images *current-animation*)
                 do (setf (buffer-brightness (led-frame image)) brightness)))
         (display-frame output (next-image *current-animation*)))
        (t
         (sleep .01))))))

(defvar *frame-thrower-thread* nil)

(defun start ()
  (when (and *frame-thrower-thread*
             (not (ccl:process-exhausted-p *frame-thrower-thread*)))
    (error "frame thrower already running"))
  (cl-log:log-message :info "Starting frame-thrower agent")
  (messaging:make-agent :display 'display-loop))
