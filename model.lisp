(in-package :clobber)

(defun player-location ()
  (cons (x-pos *player*) (y-pos *player*)))

(defun player-layer ()
  (layer *player*))

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