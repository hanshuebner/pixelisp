;; -*- Lisp -*-

(defpackage :clock
  (:use :cl :alexandria))

(in-package :clock)

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

(defun render-time (digits-file output-file hours minutes seconds)
  (let* ((input-stream (skippy:load-data-stream digits-file))
         (digits-image (aref (skippy:images input-stream) 0))
         (output-image (skippy:make-image :width 16
                                          :height 16
                                          :color-table (skippy:copy-color-table (skippy:color-table input-stream))))
         (stream (skippy:make-data-stream :width 16 :height 16)))
    (when (> hours 9)
      (render-digit digits-image output-image 0 (floor hours 10)))
    (render-digit digits-image output-image 1 (mod hours 10))
    (render-digit digits-image output-image 2 (floor minutes 10))
    (render-digit digits-image output-image 3 (mod minutes 10))
    (destructuring-bind (x y) (second-positon seconds)
      (setf (skippy:pixel-ref output-image x y)
            (skippy:pixel-ref digits-image x (+ y 176))))
    (skippy:add-image output-image stream)
    (skippy:output-data-stream stream output-file)))
