;; -*- Lisp -*-

(defpackage :handlers
  (:use :cl :alexandria))

(in-package :handlers)

(defparameter *html-directory* #P"html/")
(defparameter *gifs-directory* #P"gifs/")

(defvar *gif-folder-dispatcher* (hunchentoot:create-folder-dispatcher-and-handler "/gif/"
                                                                                  *gifs-directory*))

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

(defmacro page ((name uri title) &body body)
  `(with-output-to-string (*html-output*)
     (with-html5 (*html-output*)
       (:head
        ((:meta :charset "utf-8"))
        ((:meta :http-equiv "X-UA-Compatible" :content "IE=Edge"))
        ((:meta :name "Viewport" :content "width=device-width, initial-scale=1"))
        (:title ,title)
        (dolist (stylesheet (list "bootstrap.min" "bootstrap-slider.min"
                                  "ie10-viewport-bug-workaround" "styles"
                                  (string-downcase (symbol-name ,name))))
          (when (probe-file (make-pathname :name stylesheet
                                           :type "css"
                                           :defaults (merge-pathnames #P"css/" *html-directory*)))
            (html ((:link :rel "stylesheet" :href (str "/css/" stylesheet ".css")))))))
       (:body
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

                  ((equal uri* ,uri)
                   (html ((:li :class "active") ((:a :href ,uri) (:princ title)))))

                  (t
                   (html (:li ((:a :href uri*) (:princ title)))))))))
           ((:ul :class "nav navbar-nav navbar-right")
            ((:li :class "dropdown")
             ((:a :href "#" :class "dropdown-toggle" :id "system-menu-drop" :data-toggle "dropdown" :aria-haspopup "true" :aria-expanded "false")
              " System "
              ((:span :class "caret")))
             ((:ul :class "dropdown-menu" :aria-labelledby "system-menu-drop")
              (:li ((:a :href "/system/processes") "Processes"))
              (:li ((:a :href "/system/log") "Log"))))))))
        ((:div :class "container")

         ((:div :class "game-frame")
          ,@body))
        ((:script :src "/js/jquery.min.js"))
        (:script "window.jQuery || document.write('<script src=\"/js/jquery.min.js\"></script>')")
        (dolist (js (list "bootstrap.min" "bootstrap-slider.min" "ie10-viewport-bug-workaround"
                          (string-downcase (symbol-name ,name))))
          (when (probe-file (make-pathname :name js
                                           :type "js"
                                           :defaults (merge-pathnames #P"js/" *html-directory*)))
            (html ((:script :src (str "/js/" js ".js"))))))))))

(defmacro html (&body body)
  `(xhtml-generator:html
     ,@body))

(define-main-page (home "Home" "/")
  (when-let (current-animation (display:current-animation))
    (html
      ((:img :id "current-image"
             :src (format nil "/gif/~A.gif" (display:name current-animation))
             :height 512))
      ((:div :id "current-image-name") (:princ (display:name current-animation))))))

(define-main-page (gifs "GIFs" "/gifs")
  (:form
   (dolist (image (sort (directory (make-pathname :name :wild :type "gif"
                                                  :defaults *gifs-directory*))
                        'string-lessp
                        :key #'pathname-name))
     (html ((:div :class "image-preview")
            ((:input :type "checkbox"))
            ((:img :src (format nil "/gif/~A" (file-namestring image))
                   :height 64
                   :title (pathname-name image)
                   :data-image-name (pathname-name image))))))))

(defun import-gif (input-file output-file)
  (handler-case
      (utils:run-program "gifsicle" "--resize" "_x16"
                         "-i" (namestring input-file)
                         "-o" (namestring output-file))
    (error (e)
      (cl-log:log-message :error "Error importing ~A to ~A: ~A" input-file output-file e))))

(define-main-page (upload "Upload" "/upload")
  ((:form :method "POST" :enctype "multipart/form-data")
   (when-let (file (hunchentoot:post-parameter "file"))
     (destructuring-bind (path file-name content-type) file
       (cl-log:log-message :info "File ~A uploaded to ~A, content-type ~A" file-name path content-type)
       (html (:fieldset
              (:legend "Upload result")
              (cond
                ((not (equal content-type "image/gif"))
                 (html ((:div :class "alert alert-danger")
                        "Invalid file type, only GIF is supported")))

                (t
                 (let ((gif-pathname (make-pathname :name (pathname-name file-name)
                                                    :type "gif"
                                                    :defaults *gifs-directory*)))
                   (import-gif path gif-pathname)
                   (if (probe-file gif-pathname)
                       (html ((:div :class "alert alert-success")
                              "File has been imported")
                         ((:img :id "current-image"
                                :src (format nil "/gif/~A.gif" (pathname-name gif-pathname))
                                :height 256)))
                       (html ((:div :class "alert alert-danger")
                              "File could not be imported (gifsicle failed?)"))))))))))
   (html
     (:fieldset
      (:legend "Upload a new GIF")
      ((:div :class "form-group")
       ((:label :class "label") "Upload a GIF file")
       ((:input :type "file" :name "file")))
      ((:div :class "form-group")
       ((:button :type "submit" :class "btn btn-primary") "Upload"))))))

(define-main-page (settings "Settings" "/settings")
  (:form
   (:fieldset
    (:legend "Brightness")
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
     (:label "Bright")))
   (:fieldset
    (:legend "Animation Speed")
    ((:div :class "form-group")
     ((:label :class "left-slider-label") "Rohypnol")
     (:princ "&nbsp;&nbsp;&nbsp;")
     ((:input :id "chill-factor"
              :data-slider-id "chill-factor"
              :type "text"
              :data-slider-min "0"
              :data-slider-max "5"
              :data-slider-step "0.1"
              :data-slider-value (format nil "~G" (storage:config 'display:chill-factor))))
     (:princ "&nbsp;&nbsp;&nbsp;")
     (:label "Amphetamin")))))

(defclass sse-event ()
  ((event :initform nil :initarg :event :reader event)
   (data :initform nil :initarg :data :reader data)
   (id :initform nil :initarg :id :reader id)
   (retry :initform nil :initarg :retry :reader retry)))

(defmethod print-object ((event sse-event) stream)
  (print-unreadable-object (event stream :type t)
    (format stream "EVENT: ~A DATA: ~S" (event event) (data event))))

(defmethod event-string ((event sse-event))
  (format nil
          "~@[event: ~(~A~)~%~]~
           ~:[:~;~:*data: ~A~]~%~
           ~@[id: ~A~%~]~
           ~@[retry: ~A~%~]~
           ~%"
          (event event)
          (data event)
          (id event)
          (retry event)))

(defclass sse-idle-event ()
  ())

(defmethod print-object ((event sse-idle-event) stream)
  (print-unreadable-object (event stream :type t)))

(defmethod event-string ((event sse-idle-event))
  (format nil ": idle~%~%"))

(defvar *client-id* 0)

(defclass sse-client ()
  ((stream :initarg :stream :reader stream)
   (lock :initform (ccl:make-lock "event-stream-lock") :accessor lock)
   (id :initform (ccl::atomic-incf *client-id*) :reader id)))

(defmethod print-object ((client sse-client) stream)
  (print-unreadable-object (client stream :type t)
    (format stream "ID: ~A" (id client))))

(defmethod send-event ((client sse-client) event)
  (cl-log:log-message :info "Sending SSE event ~A to client ~A" event client)
  (ccl:with-lock-grabbed ((lock client))
    (write-string (event-string event) (stream client))
    (finish-output (stream client))))

(hunchentoot:define-easy-handler (events :uri "/events") ()
  (setf (hunchentoot:content-type*) "text/event-stream")
  (let ((client (make-instance 'sse-client
                               :stream (flexi-streams:make-flexi-stream (hunchentoot:send-headers)))))
    (cl-log:log-message :info "Event client ~A connected" client)
    (events:subscribe (lambda (event)
                        (cl-log:log-message :info "Forwarding event ~A to client ~A"
                                            event client)
                        (send-event client (make-instance 'sse-event
                                                          :event (events:type event)
                                                          :data (events:data event)
                                                          :id (events:id event)))))
    (handler-case
        (loop
          (send-event client (make-instance 'sse-idle-event))
          (sleep 10))
      (ccl:socket-error (e)
        (declare (ignore e))
        (cl-log:log-message :info "Event client ~A disconnected" client)))))

(hunchentoot:define-easy-handler (load-gif :uri "/load-gif") (name)
  (messaging:send :display
                  :set-animation (display:load-gif (make-pathname :name name
                                                                  :defaults (make-pathname :type "gif"
                                                                                           :defaults *gifs-directory*))))
  "loaded")

(defun parse-float (string)
  (float (parse-number:parse-positive-real-number string)))

(hunchentoot:define-easy-handler (chill :uri "/chill") ((factor :parameter-type 'parse-float))
  (when (eq (hunchentoot:request-method*) :post)
    (check-type factor (float 0.0 5.0))
    (setf (storage:config 'display:chill-factor) factor))
  (format nil "chill factor ~A" (storage:config 'display:chill-factor)))

(hunchentoot:define-easy-handler (brightness :uri "/brightness") ((level :parameter-type 'integer))
  (when (eq (hunchentoot:request-method*) :post)
    (check-type level (integer 0 7))
    (setf (storage:config 'display:brightness) level))
  (format nil "brightness level ~A" (storage:config 'display:brightness)))

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
  (let ((client (make-instance 'sse-client
                               :stream (flexi-streams:make-flexi-stream (hunchentoot:send-headers)))))
    (handler-case
        (loop
          (send-event client (make-instance 'sse-event
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
