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
           collect (list (nth i slots))))))


(defmacro defcontainer (name
                        &key
                        (slots 6))
  `(defclass ,name (unit) 
     ,(loop for i from 0 to slots
         collect (list (intern (concatenate 'string "slot-" (write-to-string i)))))))

(defmacro defplayer (&key
                     (name "John Galt")
                     (hp 100)
                     (str 10))
  `(defclass player (unit)
     ((name :initform ,name)
      (hp :initform ,hp)
      (str :initform ,str)
      (bag :initform (make-instance 'rucksack)))))

;;; We'll move these to objects.lisp after or something
(defobject unit (x y hp inventory))

(defobject earth () :inherit (unit)) ; 0
(defobject stone () :inherit (unit)) ; 1
(defobject water () :inherit (unit)) ; 2
(defobject dirt  () :inherit (unit)) ; 3
(defobject fire  () :inherit (unit)) ; 4
(defobject clay  () :inherit (unit)) ; 5
(defobject air   () :inherit (unit)) ; 6

(defcontainer rucksack :slots 16) ; 7
(defplayer)
(defparameter *player* (make-instance 'player))

(defmacro deftable (holder-name)
  (let ((slots))
    (maphash #'(lambda (key value)
                 (declare (ignore key))
                 (push (list value
                             :type 'simple-vector
                             :initform 'nil
                             :accessor value) slots)) *object-lookup-table*)
    `(defclass ,holder-name ()
       ,slots)))

(defgeneric add (holder obj)
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