;; -*- Lisp -*-

(defpackage :handlers
  (:use :cl :alexandria))

(in-package :handlers)

(defparameter *html-directory* #P"html/")

(defvar *gif-folder-dispatcher* (hunchentoot:create-folder-dispatcher-and-handler "/gif/"
                                                                                  (gallery:make-gif-pathname nil)))

(pushnew *gif-folder-dispatcher* hunchentoot:*dispatch-table*)

(defvar *html-output*)

(defmacro with-html5 ((stream) &body body)
  `(progn
     (format ,stream "<!DOCTYPE html>~%")
     (let ((xhtml-generator:*html-sink* (cxml:make-character-stream-sink ,stream :canonical nil)))
       (xhtml-generator:html
         ((:html :language "en")
          ,@body))
       (sax:end-document xhtml-generator:*html-sink*))))

(defun str (&rest strings)
  (apply 'concatenate 'string strings))

(defvar *pages* nil)

(defun register-page (function-name name uri)
  (let ((entry (list function-name name uri)))
    (if-let (where (member uri *pages* :key #'third :test #'equal))
      (setf (car where) entry)
      (appendf *pages* (list entry)))))

(defmacro define-main-page ((name title uri) &body body)
  `(progn
     (register-page ',name ,title ,uri)
     (hunchentoot:define-easy-handler (,name :uri ,uri) ()
       (page (',name ,uri ,title)
         ,@body))))

(defmacro html (&body body)
  `(xhtml-generator:html
     ,@body))

(defun do-with-page (name uri title function)
  (with-output-to-string (*html-output*)
    (with-html5 (*html-output*)
      (:head
       ((:meta :charset "utf-8"))
       ((:meta :http-equiv "X-UA-Compatible" :content "IE=Edge"))
       ((:meta :name "Viewport" :content "width=device-width, initial-scale=1"))
       (:title "GAME FRAME: "(:princ-safe title))
       (dolist (stylesheet (list "jquery-ui.min" "jquery-ui.structure.min" "jquery-ui.theme.min"
                                 "bootstrap.min" "bootstrap-slider.min"
                                 "ie10-viewport-bug-workaround" "styles"
                                 (string-downcase (symbol-name name))))
         (when (probe-file (make-pathname :name stylesheet
                                          :type "css"
                                          :defaults (merge-pathnames #P"css/" *html-directory*)))
           (html ((:link :rel "stylesheet" :href (str "/css/" stylesheet ".css")))))))
      ((:body :class (format nil "~@[raw~]" (hunchentoot:parameter "raw")))
       (if (hunchentoot:parameter "raw")
           (funcall function)
           (html
             ((:nav :class "navbar navbar-inverse navbar-fixed-top")
              ((:div :class "container-fluid")
               ((:div :class "navbar-header")
                ((:button :type "button"
                          :class "navbar-toggle collapsed"
                          :data-toggle "collapse"
                          :data-target "#navbar"
                          :aria-expanded "false"
                          :aria-controls "navbar")
                 ((:span :class "sr-only") "Toggle Navigation")
                 ((:span :class "icon-bar"))
                 ((:span :class "icon-bar"))
                 ((:span :class "icon-bar")))
                ((:a :class "navbar-brand" :href "/") "Game Frame"))
               ((:div :id "navbar" :class "collapse navbar-collapse")
                ((:ul :class "nav navbar-nav")
                 (dolist (page *pages*)
                   (destructuring-bind (title uri*) (rest page)
                     (cond
                       ((equal uri* "/") nil)

                       ((equal uri* uri)
                        (html ((:li :class "active") ((:a :href uri) (:princ title)))))

                       (t
                        (html (:li ((:a :href uri*) (:princ title)))))))))
                ((:ul :class "nav navbar-nav navbar-right")
                 ((:li :class (format nil "dropdown~@[ ~A~]" (when (cl-ppcre:scan "^/system" uri) "active")))
                  ((:a :href "#" :class "dropdown-toggle" :id "system-menu-drop"
                       :data-toggle "dropdown" :aria-haspopup "true" :aria-expanded "false")
                   " System "
                   ((:span :class "caret")))
                  ((:ul :class "dropdown-menu" :aria-labelledby "system-menu-drop")
                   ((:li :class (format nil "~:[~;active~]" (equal uri "/system/processes")))
                    ((:a :href "/system/processes") "Processes"))
                   ((:li :class (format nil "~:[~;active~]" (equal uri "/system/log")))
                    ((:a :href "/system/log") "Log"))))
                 (:li ((:a :href "#") ((:img :class "power-icon" :src "/images/power.png"))))))))
             ((:div :class "container")
              ((:div :class "pixelisp")
               (funcall function)))))
       ((:script :src "/js/jquery.min.js"))
       (:script "window.jQuery || document.write('<script src=\"/js/jquery.min.js\"></script>')")
       (dolist (js (list "jquery-ui.min" "bootstrap.min" "bootstrap-slider.min" "ie10-viewport-bug-workaround"
                         "bootbox.min" "jquery.idle.min" "all"
                         (string-downcase (symbol-name name))))
         (when (probe-file (make-pathname :name js
                                          :type "js"
                                          :defaults (merge-pathnames #P"js/" *html-directory*)))
           (html ((:script :src (str "/js/" js ".js"))))))))))

(defmacro page ((name uri title) &body body)
  `(do-with-page ,name ,uri ,title (lambda () (html ,@body))))

(define-main-page (home "Home" "/")
  ((:div :id "frame")
   (dotimes (y 16)
     (html (:div
            (dotimes (x 16)
              (html ((:div :class "pixel" :id (format nil "pixel-~D"
                                                      (+ (* y 16)
                                                         (if (evenp y)
                                                             x
                                                             (- 15 x))))) " ")))))))
  ((:form :class "form form-inline" :id "script-form" :action "#")
   ((:div :class "form-group")
    ((:label :for "script") "Mode: ")
    ((:select :name "script")
     (dolist (script (controller:all-scripts))
       (if (equal script (controller:script))
           (html ((:option :value script :selected "selected") (:princ-safe script)))
           (html ((:option :value script) (:princ-safe script)))))))))

(defun render-image (image classes)
  (html ((:div :class classes)
         ((:div :class "overlay")
          ((:button :class "delete")
           ((:img :src "/images/delete.png"
                  :width 16 :height 16))))
         ((:img :class "gf-thumbnail"
                :src (format nil "/gif/~A" (file-namestring image))
                :height 64
                :title (pathname-name image)
                :data-image-name (pathname-name image))))))

(define-main-page (gallery "Gallery" "/gallery")
  ((:form :id "upload-form" :method "POST" :enctype "multipart/form-data")
   (:fieldset
    (:legend "Animation library")
    ((:div :class "form-group")
     (when-let (file (hunchentoot:post-parameter "file"))
       (destructuring-bind (path file-name content-type) file
         (cl-log:log-message :info "File ~A uploaded to ~A, content-type ~A" file-name path content-type)
         (html (cond
                 ((not (equal content-type "image/gif"))
                  (html ((:div :class "popup-message alert alert-danger")
                         "Invalid file type, only GIF is supported")))

                 (t
                  (let* ((gif-pathname (gallery:make-gif-pathname (pathname-name file-name)))
                         (name (pathname-name gif-pathname)))
                    (gallery:import-gif path gif-pathname)
                    (if (probe-file gif-pathname)
                        (html ((:div :class "popup-message alert alert-success")
                               ((:img :src (format nil "/gif/~A" (file-namestring gif-pathname))
                                      :class "gf-thumbnail"
                                      :height 54
                                      :title name))
                               "Animation " (:princ-safe name) " has been imported"))
                        (html ((:div :class "popup-message alert alert-danger")
                               "File could not be imported (gifsicle failed?)")))))))))
     ((:input :type "file" :id "file" :class "file-input" :name "file"))
     ((:label :class "btn btn-primary" :for "file")
      "Upload new animation"))
    ((:div :class "image-container palette")
     (:div ((:div :class "title") "All"))
     (dolist (image (sort (directory (gallery:make-gif-pathname :wild))
                          'string-lessp
                          :key #'pathname-name))
       (render-image image "image available"))))
   (:fieldset
    (:legend "Playlist")
    ((:div :class "image-container")
     ((:div :class "user-image-container")
      (dolist (image-name (gallery:playlist))
        (let ((pathname (gallery:make-gif-pathname image-name)))
          (when (probe-file pathname)
            (render-image pathname "image")))))))
   (:fieldset
    (:legend "Settings")
    ((:div :class "form-group")
     ((:label :class "left-slider-label") "Dark")
     (:princ "&nbsp;&nbsp;&nbsp;")
     ((:input :id "brightness"
              :data-slider-id "brightness"
              :type "text"
              :data-slider-min "1"
              :data-slider-max "7"
              :data-slider-step "1"
              :data-slider-value (format nil "~A" (storage:config 'display:brightness))))
     (:princ "&nbsp;&nbsp;&nbsp;")
     (:label "Bright"))
    ((:div :class "form-group")
     ((:label :class "left-slider-label") "Flunitrazepam")
     (:princ "&nbsp;&nbsp;&nbsp;")
     ((:input :id "chill-factor"
              :data-slider-id "chill-factor"
              :type "text"
              :data-slider-min "0"
              :data-slider-max "5"
              :data-slider-step "0.1"
              :data-slider-value (format nil "~G" (storage:config 'gallery:chill-factor))))
     (:princ "&nbsp;&nbsp;&nbsp;")
     (:label "Amphetamine")))))

(define-main-page (clock "Clock" "/clock")
  ((:form :id "clock-form" :action "#")
   (:fieldset
    (:legend "Clock Style")
    ((:div :class "form-group" :id "clock-styles")
     ((:div :class "radio")
      (dotimes (i 5)
        (let ((style (1+ i)))
          (html
            (:label (if (= (clock:style) style)
                        (html ((:input :type "radio"
                                       :name "style"
                                       :value style
                                       :checked "checked")))
                        (html ((:input :type "radio"
                                       :name "style"
                                       :value style))))
                    ((:img :src (format nil "/clock/preview?style=~A" style)
                           :width "128"
                           :height "128"))))))))
    (:legend "Settings")
    ((:div :class "form-group" :id "clock-settings")
     ((:div :class "checkbox" :id "render-seconds")
      (:label (if (clock:render-seconds-p)
                  (html ((:input :type "checkbox" :checked "checked") " Render seconds"))
                  (html ((:input :type "checkbox") " Render seconds")))))))))

(defun process-list-json (&key indent)
  (yason:with-output-to-string* (:indent indent)
    (yason:with-object ()
      (yason:with-object-element ("processes")
        (yason:with-array ()
          (dolist (process (sort (ccl:all-processes) #'string-lessp :key #'ccl:process-name))
            (yason:with-object ()
              (yason:encode-object-elements
               "name" (string (ccl:process-name process))
               "type" (string (type-of process))
               "whostate" (ccl:process-whostate process)
               "allocation-quantum" (ccl:process-allocation-quantum process)
               "serial-number" (ccl:process-serial-number process)
               "creation-time" (ccl:process-creation-time process)
               "priority" (ccl:process-priority process)))))))))

(defun process-monitor-handler (interval)
  (assert (plusp interval))
  (setf (hunchentoot:content-type*) "text/event-stream")
  (cl-log:log-message :info "Process monitor client connected")
  (let ((client (make-instance 'sse:client
                               :stream (flexi-streams:make-flexi-stream (hunchentoot:send-headers)))))
    (handler-case
        (loop
          (sse:send-event client (make-instance 'sse:event
                                                :event "process-list"
                                                :data (process-list-json)))
          (sleep interval))
      (ccl:socket-error (e)
        (declare (ignore e))
        (cl-log:log-message :info "Process monitor client ~A disconnected" client)))))

(hunchentoot:define-easy-handler (processes :uri "/processes") ((interval :parameter-type 'integer))
  (if interval
      (process-monitor-handler interval)
      (progn
        (setf (hunchentoot:content-type*) "application/json")
        (process-list-json :indent t))))

(hunchentoot:define-easy-handler (system-processes :uri "/system/processes") ()
  (page ('system-processes "/system/processes" "System Processes")
    ((:table :class "table")
     (:tr
      (:th "ID")
      (:th "Name")
      (:th "Type")
      (:th "State")
      (:th "Priority"))
     (dolist (process (sort (ccl:all-processes) #'< :key #'ccl:process-serial-number))
       (html
         (:tr
          (:td (:princ (ccl:process-serial-number process)))
          (:td (:princ (ccl:process-name process)))
          (:td (:princ (string (type-of process))))
          (:td (:princ (string (ccl:process-whostate process))))
          (:td (:princ (ccl:process-priority process)))))))))

(defun format-log-timestamp (timestamp)
  (local-time:format-timestring nil
                                (local-time:universal-to-timestamp (cl-log:timestamp-universal-time timestamp))
                                :format '((:hour 2) ":" (:min 2) ":" (:sec 2))))

(hunchentoot:define-easy-handler (system-log :uri "/system/log") ()
  (page ('system-log "/system/log" "System Log")
    ((:table :class "table table-sm")
     (:tr
      (:th "Time")
      (:th "Category")
      (:th "Text"))
     (dolist (message (reverse (logging:last-messages)))
       (html
         (:tr
          (:td (:princ (format-log-timestamp (cl-log:message-timestamp message))))
          ((:td :class (format nil "log-category-~(~A~)" (cl-log:message-category message)))
           (:princ (string (cl-log:message-category message))))
          (:td (:princ-safe (apply 'format
                                   nil
                                   (cl-log:message-description message)
                                   (cl-log:message-arguments message))))))))))
