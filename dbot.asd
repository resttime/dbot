(in-package #:cl-user)

(asdf:defsystem dbot
  :description "Discord Bot"
  :author "resttime"
  :version "0.0.1"
  :depends-on (#:lispcord
               #:bordeaux-threads
               #:dexador
               #:jsown)
  :serial t
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "factorio")
                 (:file "dbot")))))
