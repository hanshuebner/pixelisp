;; -*- Lisp -*-

(defpackage :handlers
  (:use :cl :alexandria))

(in-package :handlers)

(defvar *gif-folder-dispatcher* (hunchentoot:create-folder-dispatcher-and-handler "/gif/" #P"gifs/"))

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

(defmacro define-main-page ((function-name name uri) &body body)
  `(progn
     (register-page ',function-name ,name ,uri)
     (hunchentoot:define-easy-handler (,function-name :uri ,uri) ()
       (page (,uri ,name)
         ,@body))))

(defmacro page ((uri name) &body body)
  `(with-output-to-string (*html-output*)
     (with-html5 (*html-output*)
       (:head
        ((:meta :charset "utf-8"))
        ((:meta :http-equiv "X-UA-Compatible" :content "IE=Edge"))
        ((:meta :name "Viewport" :content "width=device-width, initial-scale=1"))
        (:title ,name)
        (dolist (stylesheet '("bootstrap.min" "ie10-viewport-bug-workaround" "styles"))
          (html ((:link :rel "stylesheet" :href (str "css/" stylesheet ".css"))))))
       (:body
        ((:nav :class "navbar navbar-inverse navbar-fixed-top")
         ((:div :class "container")
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
                   (html (:li ((:a :href uri*) (:princ title))))))))))))
        ((:div :class "container")
         ((:div :class "game-frame")
          ,@body))
        ((:script :src "js/jquery.min.js"))
        (:script "window.jQuery || document.write('<script src=\"js/jquery.min.js\"></script>')")
        ((:script :src "js/bootstrap.min.js"))
        ((:script :src "js/ie10-viewport-bug-workaround.js"))
        ((:script :src "js/frontend.js"))))))

(defmacro html (&body body)
  `(xhtml-generator:html
     ,@body))

(define-main-page (home "Home" "/")
  (when-let (current-animation (leds:current-animation))
    (html
      ((:img :id "current-image"
             :src (format nil "/gif/~A.gif" (leds:name current-animation))
             :height 512))
      ((:div :id "current-image-name") (:princ (leds:name current-animation))))))

(define-main-page (gifs "GIFs" "/gifs")
  (dolist (image (sort (directory #"gifs/*.gif")
                       'string-lessp
                       :key #'pathname-name))
    (html (:p ((:div :class "image-preview")
               ((:img :src (format nil "/gif/~A" (file-namestring image))
                      :height 64)))
              (:princ (pathname-name image))))))

(define-main-page (settings "Settings" "/settings")
  )

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
   (lock :initform (bt:make-lock "event-stream-lock") :accessor lock)
   (id :initform (ccl::atomic-incf *client-id*) :reader id)))

(defmethod print-object ((client sse-client) stream)
  (print-unreadable-object (client stream :type t)
    (format stream "ID: ~A" (id client))))

(defmethod send-event ((client sse-client) event)
  (cl-log:log-message :info "Sending SSE event ~A to client ~A" event client)
  (bt:with-lock-held ((lock client))
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
