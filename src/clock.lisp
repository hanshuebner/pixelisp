;; -*- Lisp -*-

(defpackage :clock
  (:use :cl :alexandria)
  (:export #:run
           #:style))

(in-package :clock)

(defparameter *lib-directory* #P"lib/clock/")

(storage:defconfig 'style 1)

(defun style ()
  (storage:config 'style))

(defun (setf style) (style)
  (check-type style (integer 1 5))
  (setf (storage:config 'style) style))

(defun render-digit (digits-image output-image position digit)
  (dotimes (y 16)
    (dotimes (x 3)
      (setf (skippy:pixel-ref output-image
                              (+ x
                                 (* position 4)
                                 (if (> position 1) 1 0))
                              y)
            (skippy:pixel-ref digits-image (1+ x) (+ y (* digit 16)))))))

(defun second-positon (seconds)
  (let ((seconds (mod (+ seconds 8) 60)))
    (cond
      ((< seconds 16) (list seconds 0))
      ((< seconds 30) (list 15 (- seconds 15)))
      ((< seconds 46) (list (- 15 (- seconds 30)) 15))
      (t (list 0 (- 15 (- seconds 45)))))))

(defun render-time (digits-image color-table hours minutes seconds)
  (let ((output-image (skippy:make-image :width 16
                                         :height 16
                                         :color-table color-table)))
    (when (> hours 9)
      (render-digit digits-image output-image 0 (floor hours 10)))
    (render-digit digits-image output-image 1 (mod hours 10))
    (render-digit digits-image output-image 2 (floor minutes 10))
    (render-digit digits-image output-image 3 (mod minutes 10))
    (destructuring-bind (x y) (second-positon seconds)
      (setf (skippy:pixel-ref output-image x y)
            (skippy:pixel-ref digits-image x (+ y 176))))
    output-image))

(defun render-time-to-leds (digits-image color-table hours minutes seconds)
  (let ((output-image (render-time digits-image color-table hours minutes seconds)))
    (display:image-to-leds output-image color-table)))

(defun get-current-time ()
  (multiple-value-bind (seconds minutes hours) (decode-universal-time (get-universal-time))
    (list hours minutes seconds)))

(defun read-digits (style)
  (let* ((digits-file (make-pathname :name (format nil "digits_~A" style)
                                     :type "gif"
                                     :defaults *lib-directory*))
         (input-stream (skippy:load-data-stream digits-file))
         (digits-image (aref (skippy:images input-stream) 0))
         (color-table (skippy:copy-color-table (skippy:color-table input-stream))))
    (values digits-image color-table)))

(defun run ()
  (loop
    (with-simple-restart (app:start "(Re)start the clock")
      (let* ((style (style))
             previous-time)
        (multiple-value-bind (digits-image color-table) (read-digits style)
          (loop
            (let ((current-time (get-current-time)))
              (unless (equal current-time previous-time)
                (messaging:send :display
                                :set-frame-buffer
                                (apply #'render-time-to-leds digits-image color-table current-time))
                (setf previous-time current-time))
              (unless (equal style (style))
                (invoke-restart 'app:start))
              (sleep 0.1))))))))

(hunchentoot:define-easy-handler (clock-style :uri "/clock/style") ((style :parameter-type 'integer))
  (when (eq (hunchentoot:request-method*) :post)
    (setf (style) style))
  (princ-to-string (style)))

(hunchentoot:define-easy-handler (clock-preview :uri "/clock/preview") ((style :parameter-type 'integer))
  (check-type style (integer 1 5))
  (multiple-value-bind (digits-image color-table) (read-digits style)
    (let ((image (apply #'render-time digits-image color-table (get-current-time))))
      (setf (hunchentoot:content-type*) "image/gif")
      (let ((stream (skippy:make-data-stream :width 16 :height 16 :color-table color-table)))
        (skippy:add-image image stream)
        (flex:with-output-to-sequence (s)
          (skippy:write-data-stream stream s))))))
