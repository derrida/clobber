(in-package :clobber)

;; Assets
(defparameter *grass-sprite-path*
  #p"grass.png")
(defvar *grass-sprite*)

;; TILES

(defclass tile ()
  ((blocks-light :accessor blocks-light :initform nil)
   (blocks-move :accessor blocks-move :initform nil)
   (sprite :accessor sprite :initform nil :initarg :sprite)))

(defclass burnable ()
  ((burning :accessor burning :initform nil)))

(defclass grass (tile burnable) ())
(defclass abyss (tile)
  ((blocks-light :initform t)
   (blocks-move :initform t)))

(defclass unit ()
  ((x-position :accessor x-pos :initform 0          :initarg :x     :type fixnum)
   (y-position :accessor y-pos :initform 0          :initarg :y     :type fixnum)
   (hit-points :accessor hp    :initform nil        :initarg :hp    :type fixnum)
   (color      :accessor color :initform sdl:*cyan* :initarg :color :type sdl:color)))

(defclass item (unit) ())

(defclass tool (item) ())

(defclass npc (unit)
  ((race       :accessor race :initform 'human :initarg :race)
   (x-position :initform 100)
   (y-position :initform 100)
   (weapon     :initform nil)))

(defclass player (npc) ())

(defclass mob    (npc) ())

(defparameter *random-color* sdl:*white*)
(defparameter *console* nil)
(defparameter *gameboard* (make-array '(20 20)))
(defparameter *player* (make-instance 'player :y 10 :x 10 :color *random-color*))
(defparameter *mob* (list (make-instance 'mob :x (random 200) :y (random 200))))
(defparameter *grass-tile* (make-instance 'tile :sprite nil))

(defun get-player-location ()
  (values (x-pos *player*) (y-pos *player*)))

(defgeneric render (unit x y)
  (:documentation "Renders a unit onto the default sdl window.")
  (:method ((player player) x y)
    (sdl:draw-box-* x y 2 2
                    :color (color *player*)))
  (:method ((mob mob) x y)
    (sdl:draw-box-* (x-pos *mob*) (y-pos *mob*) 2 2
                    :color (color *player*)))
  (:method ((tile tile) x y)    
    (sdl:draw-surface-at-* (sprite tile) x y)))

(defgeneric create (unit)
  (:documentation "Creates any type of unit and pushes it onto that unit's stack.")
  (:method ((player player))
    (make-instance 'player :x 200
                           :y 200
                           :color sdl:*red*))
  (:method ((mob mob))
    (push (make-instance 'mob :x (random 200)
                              :y (random 200)
                              :color sdl:*red*) *mob*)))

(defun text (string x y)
  "Writes text to the default sdl surface"
  (sdl:draw-string-solid-* string x y))

(defun render-tiles (tile)
  "This function renders  tiles across the default SDL window."
  (loop for i from 0 to 192 by 8 do
       (loop for j from 0 to 192 by 8 do
            (render tile i j))))

(defun maparray (fn arr)
  "This function maps a function across the values of an array destructively."
  (iter outer (for i below (array-dimension arr 0))
        (iter (for j below (array-dimension arr 1))
              (in outer (setf (aref arr i j) (funcall `,fn (aref arr i j)))))))

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
  (setf (sprite *grass-tile*) (sdl:load-image *grass-sprite-path*)))

(defun draw ()
  "This code is looped repeatedly while main is run. The order of drawing code is important."
  (render-tiles *grass-tile*)
  (render *player* (x-pos *player*) (y-pos *player*)))

(defun on-key-down-event (key)
  "This code is run on when an sdl key-down event is detected."
  (case key
    (:sdl-key-up (decf (y-pos *player*)))
    (:sdl-key-down (incf (y-pos *player*)))
    (:sdl-key-left (decf (x-pos *player*)))
    (:sdl-key-right (incf (x-pos *player*)))
    (:sdl-key-escape (sdl:push-quit-event))))

(defun on-button-down-event (button)  
  "These function are run when a mouse-button-down event is detected."
  (declare (ignore button))
  (when (sdl:mouse-left-p)
    (setf *random-color*
          (sdl:color :r (random 255)
                     :g (random 255)
                     :b (random 255)))))

(defun on-quit-event ()
  "Run this code when the quit-event is detected prior to exiting."
  t)

(defun on-idle-event ()
  "This code is run during the idle-event loop."
  (sdl:clear-display sdl:*black*)
  (draw)
  (sdl:update-display))

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
