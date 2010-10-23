(defsystem :menbot
  :version "0.10"
  :description "Non-Enterprise Deployment Solutions"
  :author "ramus"
  :serial t
  :depends-on (#:alexandria
               #:cl-irc
               #:split-sequence
               ; Needed by extra
               #:bk-tree
               #:cl-json
               #:cl-store
               #:cl-twit
               #:chronicity
               #:cxml
               #:drakma
               #:html-entities
               #:local-time)
  :components ((:file "packages")
               (:file "config")
               (:module core
                        :components ((:file "menbot")
                                     (:file "irc")
                                     (:file "util")))
               (:module extra
                        :components ((:file "notes")
                                     (:file "when")
                                     (:file "google")))))
