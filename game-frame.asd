(defsystem :game-frame
  :depends-on (:alexandria
               :skippy
               :flexi-streams
               :bknr.datastore
               :hunchentoot
               :drakma
               :xhtmlgen
               :queues.simple-cqueue)
  :serial t
  :components ((:file "leds")
               (:file "server")
               (:file "handlers")))
