;; -*- Lisp -*-

(defpackage :gallery
  (:use :cl :alexandria)
  (:export #:play
           #:playlist
           #:make-gif-pathname
           #:import-gif
           #:chill-factor))

(in-package :gallery)

(defparameter *gifs-directory* #P"gifs/")

(storage:defconfig 'playlist nil)
(storage:defconfig 'animation-show-time 15)
(storage:defconfig 'chill-factor 2)

(defun playlist ()
  (storage:config 'playlist))

(defun (setf playlist) (new)
  (setf (storage:config 'playlist) new)
  new)

(defun load-animation (file)
  (cl-log:log-message :info "Loading ~A" file)
  (handler-case
      (display:load-gif file :chill-factor (storage:config 'chill-factor))
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

(defun check-playlist-changed (playlist)
  (when (or (not (typep playlist (playlist-class)))
            (changedp playlist))
    (cl-log:log-message :debug "restarting because of playlist change")
    (invoke-restart 'app:start)))

(defun play ()
  (loop
    (with-simple-restart (app:start "Fresh start")
      (loop with playlist = (make-instance (playlist-class))
            with current-animation
            with animation-files = (animation-files playlist)
            with single-file-paylist-p = (= (length animation-files) 1)
            do (loop for file in animation-files
                     for animation-show-time = (storage:config 'animation-show-time)
                     for animation-end-time = (+ (get-universal-time) animation-show-time)
                     do (check-playlist-changed playlist)
                        (when (or (not current-animation)
                                  (not single-file-paylist-p))
                          (setf current-animation (load-animation file))
                          (messaging:send :display :set-animation current-animation))
                        (loop for first-start-p = t then nil
                              for remaining-time = (- animation-end-time (get-universal-time))
                              while (plusp remaining-time)
                              do (with-simple-restart (app:resume "Resume playing")
                                   (cl-log:log-message :debug "remaining animation time: ~:D seconds" remaining-time)
                                   (when (or (not first-start-p)
                                             single-file-paylist-p)
                                     (messaging:send :display :set-animation current-animation))
                                   (sleep remaining-time))
                              finally (when (and single-file-paylist-p
                                                 (not (plusp remaining-time)))
                                        (messaging:send :display :set-animation current-animation))))))))

(defun import-gif (input-file output-file)
  (handler-case
      (utils:run-program "gifsicle" "--resize" "_x16"
                         "-i" (namestring input-file)
                         "-o" (namestring output-file))
    (error (e)
      (cl-log:log-message :error "Error importing ~A to ~A: ~A" input-file output-file e))))

(hunchentoot:define-easy-handler (load-gif :uri "/load-gif") (name)
  (messaging:send :display
                  :set-animation (display:load-gif (make-gif-pathname name)))
  "loaded")

(hunchentoot:define-easy-handler (delete-gif :uri "/delete-gif") (name)
  (cond
    ((eq (hunchentoot:request-method*) :post)
     (delete-file (make-gif-pathname name))
     (format t "image ~A deleted" name))
    (t
     (error "invalid request method, need POST"))))

(hunchentoot:define-easy-handler (gallery-playlist :uri "/gallery/playlist") ()
  (when (eq (hunchentoot:request-method*) :post)
    (let ((body (hunchentoot:raw-post-data :force-text t)))
      (cl-log:log-message :debug "POST body: ~S" body)
      (setf (playlist) (yason:parse body))))
  (setf (hunchentoot:content-type*) "application/json")
  (yason:encode (playlist)))

(defun parse-float (string)
  (float (parse-number:parse-positive-real-number string)))

(hunchentoot:define-easy-handler (chill :uri "/gallery/chill") ((factor :parameter-type 'parse-float))
  (when (eq (hunchentoot:request-method*) :post)
    (check-type factor (float 0.0 5.0))
    (setf (storage:config 'chill-factor) factor)
    (when-let (animation (display:current-animation))
      (setf (display:chill-factor animation) factor)))
  (format nil "chill factor ~A" (storage:config 'chill-factor)))
