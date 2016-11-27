;; -*- Lisp -*-

(defpackage :handlers
  (:use :cl :alexandria))

(in-package :handlers)

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

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *pages* nil))

(defmacro define-main-page ((function-name name uri) &body body)
  (let ((entry (list function-name name uri)))
    (if-let (where (member uri *pages* :key #'third :test #'equal))
      (setf (car where) entry)
      (appendf *pages* (list entry))))
  `(hunchentoot:define-easy-handler (,function-name :uri ,uri) ()
     (page (,uri ,name)
       ,@body)))

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
           ((:a :class "navbar-brand" :href "/") "GAME FRAME"))
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
        ((:script :src "js/ie10-viewport-bug-workaround.js"))))))

(defmacro html (&body body)
  `(xhtml-generator:html
     ,@body))

(define-main-page (home "Home" "/")
  )

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

