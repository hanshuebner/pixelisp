(defsystem :game-frame
  :depends-on (:alexandria
               :skippy
               :flexi-streams
               :cl-store
               :hunchentoot
               :drakma
               :xhtmlgen
               :cl-log
               :local-time
               :parse-number
               :yason
               :erlangen
               :cl-ppcre)
  :serial t
  :components ((:file "utils")
               (:file "logging")
               (:file "remote")
               (:file "storage")
               (:file "events")
               (:file "display")
               (:file "server")
               (:file "handlers")))
