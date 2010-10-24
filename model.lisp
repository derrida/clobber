(in-package :clobber)

;;; TODO: 
;;; [ ]  Add slot to player class that tells us which level the
;;;      player is currently on. (altitude?)
;;;
;;; [ ]  Add slot to items which can be used to map them to a fixnum (integer)
;;;
;;; [ ]  Add function to write out the world so it can persist


(defparameter *level-0* (make-array '(10 10)
                                    :initial-contents '((0 0 0 0 0 0 0 0 0 0)
                                                        (0 0 0 0 0 0 0 0 0 0)
                                                        (0 0 0 0 0 0 0 0 0 0)
                                                        (0 0 0 0 0 0 0 0 0 0)
                                                        (0 0 0 0 0 0 0 0 0 0)
                                                        (0 0 0 0 0 0 0 0 0 0)
                                                        (0 0 0 0 0 0 0 0 0 0)
                                                        (0 0 0 0 0 0 0 0 0 0)
                                                        (0 0 0 0 0 0 0 0 0 0)
                                                        (0 0 0 0 0 0 0 0 0 0)))
  "The grassy knoll where player starts")

(defparameter *level-1* (make-array '(10 10)
                                    :initial-contents '((2 2 2 2 2 2 2 2 2 2)
                                                        (2 2 2 2 2 2 2 2 2 2)
                                                        (2 2 2 2 2 2 2 2 2 2)
                                                        (2 2 2 2 2 2 3 3 2 2)
                                                        (2 2 2 2 1 2 3 3 2 2)
                                                        (2 2 2 2 2 2 2 2 2 2)
                                                        (2 2 2 2 2 2 2 2 2 2)
                                                        (2 2 2 2 2 2 2 2 2 2)
                                                        (2 2 2 2 2 2 2 2 2 2)
                                                        (2 2 2 2 2 2 2 2 2 2)))
  "The grassy floor beneath the player where she starts")

(defparameter *world* (list *level-0* *level-1*)
  "The *world* is a list made of 10x10 levels")

(defun add-level ()
  (push (make-array '(10 10)) *levels*))

(defun reset-world ()
  (setf *level-0* (make-array '(10 10) :initial-element 1))
  (setf *world* (list *level-0* *level-1*)))

(defun value-at (x y)
  (aref (level-under-player) x y))

(defun level-under-player ()
  (nth (+ (altitude *player*) 1) *world*))

(defun level-above-player ()
  (nth (- (altitude *player*) 1) *world*))


; brainstorming
;
;(defgeneric dig (tool object)
;  (:documentation "The dig function decides the outcome of using a certain tool on an object")
;  (:method ((tool shovel) (object earth))
;    (create 'dirt)))