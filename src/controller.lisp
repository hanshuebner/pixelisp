;; -*- Lisp -*-

(defpackage :controller
  (:use :cl :alexandria)
  (:export #:start
           #:power
           #:script
           #:all-scripts
           #:pause
           #:resume))

(in-package :controller)

(defvar *script-directory* #P"scripts/")

(storage:defconfig 'script "gallery-and-clock")

(defun all-scripts ()
  (sort (mapcar #'pathname-name (directory (make-pathname :name :wild :type "lisp" :defaults *script-directory*)))
        #'string-lessp))

(defun find-script (name)
  (probe-file (make-pathname :name name :type "lisp"
                             :defaults *script-directory*)))

(defun restart ()
  (ccl:process-interrupt (messaging:agent-named :controller)
                         (lambda () (throw 'restart nil))))

(defvar *power* t)

(defun (setf power) (power)
  (cond
    ((eql power :toggle)
     (setf *power* (not *power*))
     (restart))
    ((not (eql power *power*))
     (setf *power* power)
     (restart)))
  *power*)

(defun power ()
  *power*)

(defun (setf script) (name)
  (unless (equal name (storage:config 'script))
    (setf (storage:config 'script) name)
    (restart))
  name)

(defun script ()
  (storage:config 'script))

(defun run-script ()
  (app:stop)
  (let* ((*package* (find-package :cl-user))
         (script-name (storage:config 'script))
         (script (find-script script-name)))
    (cl-log:log-message :info "Running script ~S" script)
    (messaging:send :display :set-animation (display:make-text script-name :owner (ccl:process-name (messaging:agent))))
    (messaging:wait-for :code :animation-at-start)
    (handler-case
        (progn
          (load script :verbose nil :print nil)
          (cl-log:log-message :info "Script ~S finished" script))
      (error (e)
        (cl-log:log-message :error "Error while running script ~S: ~A" script e)))))

(define-condition pause (condition)
  ())

(defvar *paused* nil)

(defun pause ()
  (unless *paused*
    (setf *paused* t)
    (ccl:process-interrupt (messaging:agent-named :controller)
                           (lambda ()
                             (signal 'pause)))))

(defun resume ()
  (when *paused*
    (setf *paused* nil)
    (messaging:send :controller :resume)))

(defun start ()
  (messaging:make-agent :controller
                        (lambda ()
                          (handler-bind
                              ((pause (lambda (e)
                                        (declare (ignore e))
                                        (cl-log:log-message :info "Paused, waiting for resume message")
                                        (app:pause-current)
                                        (messaging:send :display :blank)
                                        (messaging:wait-for :code :resume)
                                        (app:resume-current)
                                        (cl-log:log-message :info "Resuming"))))
                            (loop
                              (catch 'restart
                                (cond
                                  (*power*
                                   (run-script))
                                  (t
                                   (app:stop)
                                   (messaging:send :display :blank)
                                   (cl-log:log-message :info "Power off")
                                   (loop (sleep 1))))))))))

(hunchentoot:define-easy-handler (set-power :uri "/power") (switch)
  (when (eql (hunchentoot:request-method*) :post)
    (setf (power)
          (ecase (intern (string-upcase switch) :keyword)
            (:on t)
            (:off nil)
            (:toggle :toggle))))
  (if (power)
      "on"
      "off"))

(hunchentoot:define-easy-handler (script-handler :uri "/script") (name)
  (if (eql (hunchentoot:request-method*) :post)
    (cond
      ((find-script name)
       (setf (script) name)
       name)
      (t
       (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
       (format nil "script ~S not found" name)))
    (controller:script)))
