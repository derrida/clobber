(in-package :clobber)

(defun setup ()
  "This code is run once when (start) is run, it is a preamble to (draw).
   Things that are done here:
     sdl window is created.
     ttf fonts are initialized.
     key repeat rate is set.
     Framerate is set.
     Sprites are loaded into global variables."
  (sdl:window 200 200 :title-caption "Clobber")
  (setf (sdl:frame-rate) 60)
  (sdl:enable-unicode)
  (sdl-ttf:init-ttf)
  (sdl:enable-key-repeat 500 150)
  (initialize-object-table)
  (initialize-sprite-table))

(defun draw ()
  "This code is looped repeatedly while main is run. The order of drawing code is important."
  (render-tiles))

(defun start ()
  "This is starts the game."
  (bt:make-thread
   #'(lambda ()
       (sdl:with-init ()
           (setup)
           (sdl:with-events ()
             (:quit-event () (on-quit-event))
             (:key-down-event (:key key) (on-key-down-event key))
             (:mouse-button-down-event (:button button) (on-button-down-event button))
             (:idle () (on-idle-event)))))))
