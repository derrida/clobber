(in-package :clobber)

;;; TODO: 
;;; [ ]  Add slot to player class that tells us which level the
;;;      player is currently on. (layer?)
;;;
;;; [ ]  Add slot to items which can be used to map them to a fixnum (integer)
;;;
;;; [ ]  Add function to write out the world so it can persist


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

(defun add-layer ()
  (push (make-array '(10 10)) *world*))

(defun reset-world ()
  (setf *layer-0* (make-array '(10 10) :initial-element 1))
  (setf *world* (list *layer-0* *layer-1*)))

(defun value-at (x y)
  (aref (layer-under-player) y x))

(defun layer-value-at (layer x y)
  (aref layer y x))

(defun layer-under-player ()
  (nth (+ (layer *player*) 1) *world*))

(defun layer-at-player ()
  (nth (layer *player*) *world*))

(defun layer-above-player ()
  (nth (- (layer *player*) 1) *world*))


; brainstorming
;
;(defgeneric dig (tool object)
;  (:documentation "The dig function decides the outcome of using a certain tool on an object")
;  (:method ((tool shovel) (object earth))
;    (create 'dirt)))