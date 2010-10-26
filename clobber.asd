;;;; clobber.asd
(asdf:defsystem clobber
  :version "0.1 (unreleased)"
  :description "2 guys, 4 days, 1 roguelike engine"
  :author "Michael Simpson <mgsimpson@gmail.com> and Michael Patraw <m@p.com>"
  :license "MIT"
  :serial t
  :depends-on (:lispbuilder-sdl
               :lispbuilder-sdl-ttf
               :lispbuilder-sdl-image
               :bordeaux-threads
               :iterate)
  :components ((:file "package")
               (:file "classes")
               (:file "init")
               (:file "objects")
               (:file "sprites")
               (:file "render")
               (:file "print")
               (:file "model")
               (:file "events")
               (:file "game")))
