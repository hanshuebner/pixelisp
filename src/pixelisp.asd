(defsystem :pixelisp
  :depends-on (:alexandria
               :cl-log
               :cl-ppcre
               :cl-store
               :drakma
               :flexi-streams
               :hunchentoot
               :local-time
               :parse-number
               :safe-queue
               :skippy
               :xhtmlgen
               :yason)
  :serial t
  :components ((:file "utils")
               (:file "color")
               (:file "events")
               (:file "logging")
               (:file "messaging")
               (:file "storage")
               (:file "marquee")
               (:file "display")
               (:file "app")
               (:file "clock")
               (:file "gallery")
               (:file "controller")
               (:file "remote-control")
               (:file "alerter")
               (:file "sse")
               (:file "web-frame")
               (:file "handlers")
               (:file "webserver")
               (:file "server")))
