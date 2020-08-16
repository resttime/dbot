(push #p"~/.emacs.d/straight/repos/sly/slynk/" ASDF:*CENTRAL-REGISTRY*)
(asdf:load-system :slynk)
(slynk:create-server :port 4006 :dont-close t)
