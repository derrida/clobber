(in-package :clobber)

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

(defclass mob (unit container)
  ((race :accessor race :initform 'human :initarg :race)
   (level :initarg :level :accessor level)
   (hp :initarg :hp :accessor hp)
   (def :initarg :dex :accessor def)
   (att :initarg :str :accessor att)
   (tool :initarg nil :accessor tool)))

(defclass container ()
  ((slot-1 :initform nil)
   (slot-2 :initform nil)
   (slot-3 :initform nil)
   (slot-4 :initform nil)
   (slot-5 :initform nil)
   (slot-6 :initform nil)
   (slot-7 :initform nil)
   (slot-8 :initform nil)))

(defclass rucksack (container) ())

(defgeneric move (layer obj nx ny)
  (:method (layer (player player) nx ny)
    (setf (aref (nth layer *world*) (y-pos player) (x-pos player)) 0)
    (setf (x-pos player) nx)
    (setf (y-pos player) ny)
    (setf (aref (nth layer *world*) ny nx) 1)))


(defgeneric render (unit x y)
  (:documentation "Renders a unit onto the default sdl window.")
  (:method ((player player) x y)
    (sdl:draw-surface-at-* (lookup-sprite 'player) (* x 8) (* y 8)))
   (:method ((mob mob) x y)
     (sdl:draw-box-* (x-pos (first *mobs*)) (y-pos (first *mobs*)) 2 2
                     :color (color *player*)))
  (:method ((sprite sdl:surface) x y)    
    (sdl:draw-surface-at-* sprite x y)))

(defgeneric create (unit)
  (:documentation "Creates any type of unit and pushes it onto that unit's stack.")
  (:method ((player player))
    (make-instance 'player :x 0
                           :y 0
                           :color sdl:*red*))
  ;; (:method ((mob mob))
  ;;   (push (make-instance 'mob :x (random 200)
  ;;                             :y (random 200)
  ;;                             :color sdl:*red*) *mob*))
  )

; brainstorming
;
;(defgeneric dig (tool object)
;  (:documentation "The dig function decides the outcome of using a certain tool on an object")
;  (:method ((tool shovel) (object earth))
;    (create 'dirt)))
