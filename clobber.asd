;;;; clobber.asd

(asdf:defsystem #:clobber
  :serial t
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-ttf
               #:iterate)
  :components ((:file "package")
               (:file "clobber")))

