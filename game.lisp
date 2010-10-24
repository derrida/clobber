(in-package :clobber)

;; Assets

;; This should either be turned into a macro or another lookup table

;; Macro could look like:
;; (defmacro make-sprite (name filename)
;;            (let ((sym1 (concatenate 'string "*" name "-sprite-path*"))
;;                  (path (concatenate 'string "#p\"" filename "\""))
;;                  (sym2 (concatenate 'string "*" name "-sprite*")))
;;              (values `(format nil "(defparameter ~A ~A)" ',sym1 ,path)
;;                      `(format nil "(defvar ~A)" ,sym2))))

;; Item Dictionary
(defparameter *object-lookup-table* (make-hash-table)
  "This hash-table will contain a set of object-id-numbers/object-name pairs.")

(defun add-object-to-lookup-table (id name)
  (setf (gethash id *object-lookup-table*) name))

(defun initialize-object-lookup-table ()
  (add-object-to-lookup-table 0 'empty)
  (add-object-to-lookup-table 1 'player)
  (add-object-to-lookup-table 2 'earth)
  (add-object-to-lookup-table 3 'stone))

;; Sprite Dictionary
(defparameter *sprite-lookup-table* (make-hash-table)
  "This hash-table will contain a set of sprite-name/sprite-pathname pairs.")

(defun add-sprite-to-lookup-table (name filename)
  "This function adds a sprite/path pair to the *sprite-lookup-table* hash-table."
  (setf (gethash name *sprite-lookup-table*) (sdl:load-image filename)))

(defun initialize-sprite-lookup-table ()
  "This function initializes the *sprite-lookup-value* with it's starting values."
  (add-sprite-to-lookup-table 'empty  #p"empty.png")
  (add-sprite-to-lookup-table 'player #p"player.png")
  (add-sprite-to-lookup-table 'earth  #p"earth.png")
  (add-sprite-to-lookup-table 'stone  #p"stone.png"))

(defparameter *grass-sprite-path* #p"grass.png")
(defvar *grass-sprite*)

(defparameter *player-sprite-path* #p"player.png")
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

(defclass item (unit)
  ((owner :initform nil)))

(defclass tool (item) ())

(defclass npc (unit)
  ((race       :accessor race :initform 'human :initarg :race)
   (x-position :initform 100)
   (y-position :initform 100)
   (weapon     :initform nil)))

(defparameter rucksack '((1 . 0)
                         (2 . 0)
                         (3 . 0)
                         (4 . 0)
                         (5 . 0)
                         (6 . 0)
                         (7 . 0)
                         (8 . 0)))

;;  To generate the skill map we can ask the player a bunch
;;  of silly sort of relevant questions that deduce a set of
;;  values for (rand x y) forms ... which reminds me:
(defun rand (low high)
  (+ (random (- high low))
     low))

(declaim (inline bound))
(defun bound (number min max)
  (if (< number min)
      min
      (if (> number max)
          max
          number)))

(defclass player (npc)
  ((altitude :initform 0 :accessor altitude)
   (level :initarg :level :accessor level)
   (xp :initarg :xp :accessor xp)
   (hp :initarg :hp :accessor hp)
   (ap :initarg :ap :accessor ap)
   (wis :initarg :wis :accessor wis)
   (dex :initarg :dex :accessor dex)
   (str :initarg :str :accessor str)
   (spd :initarg :spd :accessor spd)))

(defclass mob (npc)
  ((ranged-weapon :accessor ranged-weapon)
   (poisonous :accessor poisonous)))

(defparameter *random-color* sdl:*white*)
(defparameter *console* nil)
(defparameter *gameboard* (make-array '(20 20)))
(defparameter *player* (make-instance 'player :y 0 :x 0 :color *random-color*))
(defparameter *mob* (list (make-instance 'mob :x (random 200) :y (random 200))))
(defparameter *grass-tile* (make-instance 'tile :sprite nil))

(defun get-player-location ()
  (values (x-pos *player*) (y-pos *player*)))

(defgeneric render (unit x y)
  (:documentation "Renders a unit onto the default sdl window.")
  (:method ((player player) x y)
    (sdl:draw-surface-at-* (lookup-sprite 'player) (* x 8) (* y 8)))
  (:method ((mob mob) x y)
    (sdl:draw-box-* (x-pos *mob*) (y-pos *mob*) 2 2
                    :color (color *player*)))
  (:method ((sprite sdl:surface) x y)    
    (sdl:draw-surface-at-* sprite x y)))

(defgeneric create (unit)
  (:documentation "Creates any type of unit and pushes it onto that unit's stack.")
  (:method ((player player))
    (make-instance 'player :x 0
                           :y 0
                           :color sdl:*red*))
  (:method ((mob mob))
    (push (make-instance 'mob :x (random 200)
                              :y (random 200)
                              :color sdl:*red*) *mob*)))

(defun text (string x y)
  "Writes text to the default sdl surface"
  (sdl:draw-string-solid-* string x y))

(defun render-tiles ()
  "This function renders  tiles across the default SDL window."
  (loop for i from 0 to 9 do
       (loop for j from 0 to 9 do
            (render (lookup-sprite (lookup-object (value-at j i))) (* i 8) (* j 8)))))

(defun lookup-object (id)
  (gethash id *object-lookup-table*))

(defun lookup-sprite (object)
  (gethash object *sprite-lookup-table*))

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
  (initialize-object-lookup-table)
  (initialize-sprite-lookup-table))

(defun draw ()
  "This code is looped repeatedly while main is run. The order of drawing code is important."
  (render-tiles)
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