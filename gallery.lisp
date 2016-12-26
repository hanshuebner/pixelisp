;; -*- Lisp -*-

(defpackage :gallery
  (:use :cl :alexandria)
  (:export #:play
           #:playlist
           #:make-gif-pathname))

(in-package :gallery)

(defparameter *gifs-directory* #P"gifs/")

(storage:defconfig 'playlist nil)
(storage:defconfig 'animation-show-time 15)

(defun playlist ()
  (storage:config 'playlist))

(defun (setf playlist) (new)
  (setf (storage:config 'playlist) new)
  new)

(defun set-animation (file)
  (cl-log:log-message :info "Loading ~A" file)
  (handler-case
      (messaging:send :display :set-animation (display:load-gif file))
    (error (e)
      (cl-log:log-message :error "Error loading animation ~A~%~A~%" file e)
      (sleep .5))))

(defun make-gif-pathname (name)
  (make-pathname :name name
                 :defaults (make-pathname :type (and name "gif")
                                          :defaults *gifs-directory*)))

(defclass playlist ()
  ((animation-files :initform nil :reader animation-files)))

(defgeneric changedp (playlist))

(defmethod initialize-instance :after ((playlist playlist) &key)
  (cl-log:log-message :info "playing ~A with ~D animations" (type-of playlist) (length (animation-files playlist))))

(defclass all-animations-playlist (playlist)
  ((last-update :reader last-update :initform (file-write-date (make-gif-pathname nil)))))

(defmethod initialize-instance :before ((playlist all-animations-playlist) &key)
  (let ((*random-state* (make-random-state t)))
    (setf (slot-value playlist 'animation-files) (alexandria:shuffle (directory #P"gifs/*.gif")))))

(defmethod changedp ((playlist all-animations-playlist))
  (/= (last-update playlist) (file-write-date (make-gif-pathname nil))))

(defclass user-playlist (playlist)
  ((animation-names :reader animation-names)))

(defmethod initialize-instance :before ((playlist user-playlist) &key)
  (with-slots (animation-names animation-files) playlist
    (setf animation-names (playlist)
          animation-files (mapcar #'make-gif-pathname animation-names))))

(defmethod changedp ((playlist user-playlist))
  (not (equal (animation-names playlist) (playlist))))

(defun playlist-class ()
  (if (playlist)
      'user-playlist
      'all-animations-playlist))

(defun play ()
  (loop
    (with-simple-restart (app:start "Fresh start")
      (loop with playlist = (make-instance (playlist-class))
            do (loop with previous-file
                     do (loop for file in (animation-files playlist)
                              do (when (or (not (typep playlist (playlist-class)))
                                           (changedp playlist))
                                   (cl-log:log-message :debug "restarting because of playlist change")
                                   (invoke-restart 'app:start))
                                 (restart-case
                                     (progn
                                       (unless (equal previous-file file)
                                         (set-animation file))
                                       (setf previous-file file)
                                       (sleep (storage:config 'animation-show-time)))
                                   (app:start ()
                                     (setf previous-file nil)))))))))
