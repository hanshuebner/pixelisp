;; -*- Lisp -*-

(defpackage :display
  (:use :cl :alexandria)
  (:export #:start
           #:current-animation
           #:load-gif
           #:name #:frames
           #:brightness
           #:image-to-leds
           #:make-frames
           #:make-text
           #:animation
           #:chill-factor
           #:settings))

(in-package :display)

(defparameter *leds-device* #-darwin "/dev/spidev0.0" #+darwin "/dev/null")

(defvar *current-animation* nil)

(storage:defconfig 'brightness 5)

(defun current-animation ()
  *current-animation*)

(defclass animation ()
  ((frames :initarg :frames :reader frames)
   (name :initarg :name :reader name)
   (next-frame-index :initform 0 :accessor next-frame-index)
   (chill-factor :initform 1 :initarg :chill-factor :accessor chill-factor)
   (owner :initarg :owner :initform nil :reader owner)))

(defmethod print-object ((animation animation) stream)
  (print-unreadable-object (animation stream :type t)
    (format stream "NAME: ~A (~:D frames, at ~D)"
            (name animation)
            (length (frames animation))
            (next-frame-index animation))))

(defmethod next-frame ((animation animation))
  (with-slots (frames next-frame-index) animation
    (prog1
        (aref frames next-frame-index)
      (incf next-frame-index)
      (when (= next-frame-index (length frames))
        (setf next-frame-index 0)))))

(defmethod at-start-p ((animation animation))
  (zerop (next-frame-index animation)))

(defclass frame ()
  ((led-data :initarg :led-data :reader led-data)
   (delay :initarg :delay :reader delay)))

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

(defun make-frames (stream)
  (loop with frame-buffer = (make-frame-buffer :brightness (storage:config 'brightness))
        with images = (skippy:images stream)
        with frames = (make-array (length images))
        with color-table = (skippy:color-table stream)
        for i from 0 below (length images)
        for image = (aref images i)
        do (setf frame-buffer (image-to-leds image color-table :frame-buffer frame-buffer)
                 (aref frames i) (make-instance 'frame
                                                :led-data (copy-sequence 'vector frame-buffer)
                                                :delay (/ (if (zerop (skippy:delay-time image))
                                                              10
                                                              (skippy:delay-time image))
                                                          100)))
        finally (return frames)))

(defun make-animation (name frames &rest args &key owner chill-factor)
  (declare (ignore owner chill-factor))
  (cl-log:log-message :info "Make animation ~A with ~D frames" name (length frames))
  (apply #'make-instance 'animation
         :name name
         :frames frames
         args))

(defun load-gif (file &rest args &key owner chill-factor)
  (declare (ignore owner chill-factor))
  (cl-log:log-message :info "Loading GIF file ~A" file)
  (apply #'make-animation
         (pathname-name file) (make-frames (skippy:load-data-stream file))
         args))

(defun make-text (text &rest args &key owner chill-factor color color-table)
  (declare (ignore owner chill-factor color color-table))
  (cl-log:log-message :info "Rendering text ~S" text)
  (apply #'make-animation
         "marquee" (make-frames (apply #'marquee:animate-banner
                                       (marquee:render-banner text)
                                       :allow-other-keys t
                                       args))
         :allow-other-keys t
         args))

(defun write-frame (frame output)
  (write-sequence frame output)
  (events:publish :frame (format nil "~{~2,'0X~}" (coerce frame 'list))))

(defun display-frame (output frame)
  (write-frame (led-data frame) output))

(defun display-next-frame (output animation)
  (let ((start-time (get-internal-real-time))
        (frame (next-frame animation)))
    (display-frame output frame)
    (let* ((write-duration (/ (- (get-internal-real-time) start-time) internal-time-units-per-second))
           (delay (- (* (delay frame) (chill-factor animation))
                     write-duration)))
      (when (plusp delay)
        (sleep delay)))))

(defun set-current-animation (animation)
  (setf *current-animation* animation)
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
           (loop for frame across (frames *current-animation*)
                 do (setf (buffer-brightness (led-data frame)) brightness)))
         (display-next-frame output *current-animation*)
         (when (and (at-start-p *current-animation*)
                    (owner *current-animation*))
           (messaging:send (owner *current-animation*) :animation-at-start)))
        (t
         (sleep .01))))))

(defun start ()
  (cl-log:log-message :info "Starting display agent")
  (messaging:make-agent :display 'display-loop))

(defun brightness ()
  (storage:config 'brightness))

(defun (setf brightness) (level)
  (check-type level (integer 0 7))
  (setf (storage:config 'brightness) level)
  (events:publish :brightness level))

(hunchentoot:define-easy-handler (display-brightness :uri "/display/brightness") ((level :parameter-type 'integer))
  (when (eq (hunchentoot:request-method*) :post)
    (setf (brightness) level))
  (format nil "brightness level ~A" (storage:config 'brightness)))
