(defsystem :game-frame
  :depends-on (:alexandria
               :skippy
               :flexi-streams
               :cl-store
               :hunchentoot
               :drakma
               :xhtmlgen
               :queues.simple-cqueue
               :cl-log
               :local-time
               :parse-number
               :yason)
  :serial t
  :components ((:file "utils")
               (:file "storage")
               (:file "events")
               (:file "leds")
               (:file "server")
               (:file "handlers")))
