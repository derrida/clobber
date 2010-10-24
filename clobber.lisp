(in-package :clobber)

;; Assets

;; This should either be turned into a macro or another lookup table

;; Macro could look like:
;; (defmacro make-sprite (name filename)
;;            (let ((sym1 (concatenate 'string "*" name "-sprite-path*"))
;;                  (path (concatenate 'string "#p\"" filename "\""))
;;                  (sym2 (concatenate 'string "*" name "-sprite*")))
;;              (values `(format nil "(defparameter ~A ~A)" ',sym1 ,path)
;;                      `(format nil "(defvar ~A)" ,sym2))))

(defun add-object-to-lookup-table (id name)
  (setf (gethash id *object-lookup-table*) name))

(defun initialize-object-lookup-table ()
  (add-object-to-lookup-table 0 'empty)
  (add-object-to-lookup-table 1 'player)
  (add-object-to-lookup-table 2 'earth)
  (add-object-to-lookup-table 3 'stone)
  (add-object-to-lookup-table 4 'grass))

(defun add-sprite-to-lookup-table (name filename)
  "This function adds a sprite/path pair to the *sprite-lookup-table* hash-table."
  (setf (gethash name *sprite-lookup-table*) (sdl:load-image filename)))
  ;(push (sdl:load-image filename) sdl:*default-image-path*))

(defun initialize-sprite-lookup-table ()
  "This function initializes the *sprite-lookup-table* with it's starting values."
  (add-sprite-to-lookup-table 'empty  #p"empty.png")
  (add-sprite-to-lookup-table 'player #p"player.png")
  (add-sprite-to-lookup-table 'earth  #p"earth.png")
  (add-sprite-to-lookup-table 'stone  #p"stone.png")
  (add-sprite-to-lookup-table 'grass  #p"grass.png"))

;; (defun initialize-sprites ()
;;   (maphash #'sdl:load-image *sprite-lookup-table*))

;;  To generate the skill map we can ask the player a bunch
;;  of silly sort of relevant questions that deduce a set of
;;  values for (rand x y) forms ... which reminds me:
(defun rand (low high)
  (+ (random (- high low))
     low))

(declaim (inline bound))
(defun bound (number min max)
  (if (< number min)
      min
      (if (> number max)
          max
          number)))

(defun get-player-location ()
  (values (x-pos *player*) (y-pos *player*)))

(defun render-tiles ()
  "This function renders  tiles across the default SDL window."
  (loop for i from 0 to 9 do
       (loop for j from 0 to 9 do
            (let ((val (layer-value-at (layer-at-player) i j)))
              (render (lookup-sprite (lookup-object (value-at i j))) (* i 8) (* j 8))
              (when (> val 0)
                  (render (lookup-sprite (lookup-object val)) (* i 8) (* j 8)))))))

(defun lookup-object (id)
  (gethash id *object-lookup-table*))

(defun lookup-sprite (object)
  (gethash object *sprite-lookup-table*))

(defun maparray (fn arr)
  "This function maps a function across the values of an array destructively."
  (iter outer (for i below (array-dimension arr 0))
        (iter (for j below (array-dimension arr 1))
              (in outer (setf (aref arr i j) (funcall `,fn (aref arr i j)))))))