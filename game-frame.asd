(defsystem :game-frame
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
               (:file "logging")
               (:file "messaging")
               (:file "remote-control")
               (:file "storage")
               (:file "events")
               (:file "display")
               (:file "clock")
               (:file "scripter")
               (:file "handlers")
               (:file "webserver")
               (:file "server")))
