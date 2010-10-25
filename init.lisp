(in-package :clobber)

    ;; Item Dictionary
(defparameter *object-lookup-table* (make-hash-table)
  "This hash-table will contain a set of object-id-numbers/object-name pairs.")

;; Sprite Dictionary
(defparameter *sprite-lookup-table* (make-hash-table)
  "This hash-table will contain a set of sprite-name/sprite-pathname pairs.")

(defparameter *player* (make-instance 'player :y 0 :x 0))

(defparameter *mobs* (vector (make-instance 'mob :x (random 200) :y (random 200))))

(defparameter *chests* nil)


(defparameter *layer-0* (make-array '(10 10)
                                    :initial-contents '((0 0 0 0 0 0 0 0 0 0)
                                                        (0 0 0 0 0 0 0 0 0 0)
                                                        (0 0 0 0 0 0 0 0 0 0)
                                                        (0 0 0 0 0 0 0 0 0 0)
                                                        (0 0 0 0 1 0 0 0 0 0)
                                                        (0 0 0 0 0 0 0 0 0 0)
                                                        (0 0 0 0 0 0 0 0 0 0)
                                                        (0 0 0 0 0 0 0 0 0 0)
                                                        (0 0 0 0 0 0 0 0 0 0)
                                                        (0 0 0 0 0 0 0 0 0 0)))
  "The grassy knoll where player starts")

(defparameter *layer-1* (make-array '(10 10)
                                    :initial-contents '((2 2 2 2 2 2 2 2 2 2)
                                                        (2 2 2 2 2 2 2 2 2 2)
                                                        (2 2 2 2 2 2 2 2 2 2)
                                                        (2 2 2 2 2 2 3 3 2 2)
                                                        (2 2 2 2 2 2 3 3 2 2)
                                                        (2 2 2 2 2 2 2 2 2 2)
                                                        (2 2 2 2 2 2 2 2 2 2)
                                                        (2 2 2 2 2 2 2 2 2 2)
                                                        (2 2 2 2 2 2 2 2 2 2)
                                                        (2 2 2 2 2 2 2 2 2 2)))
  "The grassy floor beneath the player where she starts")

(defparameter *world* (list *layer-0* *layer-1*)
  "The *world* is a list made of 10x10 layers")
