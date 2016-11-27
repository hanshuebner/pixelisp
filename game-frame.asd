(defsystem :game-frame
  :depends-on (:alexandria
               :skippy
               :flexi-streams
               :bknr.datastore
               :hunchentoot
               :drakma
               :xhtmlgen
               :queues.simple-cqueue
               :cl-log
               :local-time)
  :serial t
  :components ((:file "events")
               (:file "leds")
               (:file "server")
               (:file "handlers")))
