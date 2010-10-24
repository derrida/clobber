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