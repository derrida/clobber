(in-package :clobber)

(defmacro defobject (name
                     (&body slots)
                     &key
                     (inherit nil))
  (let ((number-of-slots (length slots)))
    `(defclass ,name ,(if inherit
                          inherit
                          '())
       ,(loop for i from 0 to (1- number-of-slots)
           collect (list (nth i slots) :accessor (nth i slots))))))

;;; Unit Definition
(defobject unit (layer x y hp inventory))

(defmacro defcontainer (name
                        &key
                        (slots 6))
  `(defclass ,name (unit) 
     ,(loop for i from 0 to slots
         collect (list (intern (concatenate 'string "slot-" (write-to-string i)))))))

(defmacro defplayer (&key
                     (layer 0)
                     (name "John Galt")
                     (hp 100)
                     (str 10))
  `(defclass player (unit)
     ((layer :initform ,layer :accessor layer)      
      (name :initform ,name :accessor name)
      (x-pos :initform 5 :accessor x-pos)
      (y-pos :initform 5 :accessor y-pos)
      (hp :initform ,hp :accessor hp)
      (direction :initform 0 :accessor direction)
      (str :initform ,str :accessor str)
      (bag :initform (make-instance 'rucksack) :accessor bag))))

(defcontainer rucksack :slots 16)

(defplayer)

(defparameter *player* (make-instance 'player))

(defclass world ()
  ((layers :type vector :initform (list (make-array '(10 10)) (make-array '(10 10))) :accessor layers)
   (objects :type hash-table :initform (make-hash-table) :accessor objects)
   (instances :type list :initform '() :accessor instances)
   (sprites :type hash-table :initform (make-hash-table) :accessor sprites)))

(defparameter *world* (make-instance 'world))

(defmacro deftable (table-name)
  (let ((slots))
    (maphash #'(lambda (key value)
                 (declare (ignore key))
                 (push (list value
                             :type 'simple-vector
                             :initform 'nil
                             :accessor value) slots)) *object-lookup-table*)
    `(defclass ,table-name ()
       ,slots)))

(defgeneric add (table obj)
  (:method (holder obj)
    (vector-push-extend obj `(,obj holder))))

(defgeneric move (layer obj nx ny)
  (:method (layer (player player) nx ny)
    (let ((px (x-pos player))
          (py (y-pos player)))
      (setf (aref (nth layer (layers *world*)) py px) 0)
      (setf (x-pos player) nx)
      (setf (y-pos player) ny)
      (setf (aref (nth layer (layers *world*)) ny nx) 1))))

(defgeneric render (sprite x y)
  (:documentation "Renders a object onto the default sdl window.")
  (:method (sprite x y)
    (sdl:draw-surface-at-* sprite x y)))

(defgeneric create (unit)
  (:documentation "Creates any type of unit and pushes it onto that unit's stack.")
  (:method ((player player))
    (make-instance 'player)))