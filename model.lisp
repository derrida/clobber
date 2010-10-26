(in-package :clobber)

;;; TODO: 
;;; [ ]  Add slot to player class that tells us which level the
;;;      player is currently on. (layer?)
;;;
;;; [ ]  Add slot to items which can be used to map them to a fixnum (integer)
;;;
;;; [ ]  Add function to write out the world so it can persist
(defun get-player-location ()
  (values (x-pos *player*) (y-pos *player*)))

(defun add-layer ()
  (push (make-array '(10 10)) (layers *world*)))

(defun value-at (x y)
  (aref (layer-under-player) y x))

(defun layer-value-at (layer x y)
  (aref layer y x))

(defun layer-under-player ()
  (nth (+ (layer *player*) 1) (layers *world*)))

(defun layer-at-player ()
  (nth (layer *player*) (layers *world*)))

(defun layer-above-player ()
  (nth (- (layer *player*) 1) (layers *world*)))