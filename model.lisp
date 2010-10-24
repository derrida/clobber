(in-package :clobber)

;;; TODO: 
;;; [ ]  Add slot to player class that tells us which level the
;;;      player is currently on. (layer?)
;;;
;;; [ ]  Add slot to items which can be used to map them to a fixnum (integer)
;;;
;;; [ ]  Add function to write out the world so it can persist
(defun add-layer ()
  (push (make-array '(10 10)) *world*))

(defun reset-world ()
  (setf *layer-0* (make-array '(10 10) :initial-element 0))
  (setf (aref *layer-0* 5 5) 1)
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

