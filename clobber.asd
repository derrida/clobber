;;;; clobber.asd
(asdf:defsystem #:clobber
  :serial t
  :depends-on (:lispbuilder-sdl
               :lispbuilder-sdl-ttf
               :lispbuilder-sdl-image
               :bordeaux-threads
               :iterate)
  :components ((:file "package")
               (:file "game")
               (:file "model")))
