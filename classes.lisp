(in-package :clobber)
;?
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

;;; Object Definitions
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
      (hp :initform ,hp :accessor hp)
      (str :initform ,str :accessor str)
      (bag :initform (make-instance 'rucksack) :accessor bag)))
  (defparameter *player* (make-instance 'player)))

(defclass world ()
  ((layers :type vector :initform (list (make-array '(10 10)) (make-array '(10 10))) :accessor layers)
   (objects :type hash-table :initform (make-hash-table) :accessor objects)
   (sprites :type hash-table :initform (make-hash-table) :accessor sprites)))

(defcontainer rucksack :slots 16)

(defparameter *world* (make-instance 'world))

(defplayer)

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
    (setf (aref (nth layer *world*) (y-pos player) (x-pos player)) 0)
    (setf (x-pos player) nx)
    (setf (y-pos player) ny)
    (setf (aref (nth layer *world*) ny nx) 1)))

(defgeneric render (object x y)
  (:documentation "Renders a object onto the default sdl window.")
  (:method ((sprite sdl:surface) x y)    
    (sdl:draw-surface-at-* sprite x y)))

(defgeneric create (unit)
  (:documentation "Creates any type of unit and pushes it onto that unit's stack.")
  (:method ((player player))
    (make-instance 'player)))