;; -*- Lisp -*-

(defpackage :marquee
  (:use :cl :alexandria)
  (:export
   #:animate-banner
   #:render-banner))

(in-package :marquee)

(defparameter *fonts-dir* #P"lib/fonts/")
(defparameter *default-font* "volter-goldfish")

(defun read-font-atlas (pathname)
  (let* ((stream (skippy:load-data-stream (make-pathname :type "gif" :defaults pathname)))
         (image (aref (skippy:images stream) 0)))
    (setf (skippy:color-table image) (skippy:color-table stream))
    image))

(defun read-font-char-table (pathname)
  (let ((table (make-hash-table))
        (mapping (read-from-string (alexandria:read-file-into-string pathname))))
    (loop for (char descriptor) on mapping by #'cddr
          do (setf (gethash char table) descriptor))
    table))

(defclass font ()
  ((name :initarg :name :reader name)
   (atlas :reader atlas)
   (char-table :reader char-table)))

(defmethod print-object ((font font) stream)
  (print-unreadable-object (font stream :type t :identity t)
    (format stream "NAME: ~S, ~:D glyphs" (name font) (hash-table-count (char-table font)))))

(defmethod initialize-instance :before ((font font) &key name)
  (with-slots (atlas char-table) font
    (setf atlas (read-font-atlas (make-pathname :name name :type "gif" :defaults *fonts-dir*))
          char-table (read-font-char-table (make-pathname :name name :type "lisp" :defaults *fonts-dir*)))))

(defun load-font (name)
  (make-instance 'font :name name))

(defun char-width (font char)
  (getf (gethash char (char-table font)) :width 0))

(defun char-x (font char)
  (getf (gethash char (char-table font)) :x))

(defun string-width (font string)
  (loop for char across string
        sum (char-width font char)))

(defun copy-char (font image char start-x start-y)
  (loop with char-x = (char-x font char)
        for x below (char-width font char)
        do (loop for y below (skippy:height (atlas font))
                 do (setf (skippy:pixel-ref image (+ x start-x) (+ y start-y))
                          (skippy:pixel-ref (atlas font) (+ x char-x) y)))))

(defun render-banner (text &key (font-name *default-font*))
  (let* ((font (load-font font-name))
         (image (skippy:make-image :width (+ (string-width font text) 32)
                                   :height 16
                                   :color-table (skippy:color-table (atlas font)))))
    (loop with x = 16
          with y = (floor (- 16 (skippy:height (atlas font))) 2)
          for char across text
          for width = (char-width font char)
          do (copy-char font image char x y)
             (incf x width))
    image))

(defun copy-square (banner start-x)
  (loop with square = (skippy:make-image :width 16 :height 16 :delay-time 1)
        for x below 16
        do (loop for y below 16
                 do (setf (skippy:pixel-ref square x y)
                          (skippy:pixel-ref banner (+ x start-x) y)))
        finally (return square)))

(defun animate-banner (banner-image &key (loopingp t))
  (let ((stream (skippy:make-data-stream :width 16
                                         :height 16
                                         :color-table (skippy:color-table banner-image)
                                         :loopingp loopingp)))
    (dotimes (x (- (skippy:width banner-image) 16))
      (skippy:add-image (copy-square banner-image x) stream))
    stream))

(defun save-stream (stream pathname)
  (with-open-file (s pathname :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (skippy:write-data-stream stream s)))

(defun save-gif (image pathname)
  (let ((stream (skippy:make-data-stream :width (skippy:width image)
                                         :height (skippy:height image)
                                         :color-table (skippy:color-table image))))
    (skippy:add-image image stream)
    (save-stream stream pathname)))
