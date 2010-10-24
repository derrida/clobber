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


;; Item Dictionary
(defparameter *object-lookup-table* (make-hash-table)
  "This hash-table will contain a set of object-id-numbers/object-name pairs.")

(defun add-object-to-lookup-table (id name)
  (setf (gethash id *object-lookup-table*) name))

(defun initialize-object-lookup-table ()
  (add-object-to-lookup-table 0 'empty)
  (add-object-to-lookup-table 1 'player)
  (add-object-to-lookup-table 2 'earth)
  (add-object-to-lookup-table 3 'stone)
  (add-object-to-lookup-table 4 'grass))

;; Sprite Dictionary
(defparameter *sprite-lookup-table* (make-hash-table)
  "This hash-table will contain a set of sprite-name/sprite-pathname pairs.")

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

(defparameter *player* (make-instance 'player :y 0 :x 0))
(defparameter *mobs* (list (make-instance 'mob :x (random 200) :y (random 200))))

(defun get-player-location ()
  (values (x-pos *player*) (y-pos *player*)))


(defun text (string x y)
  "Writes text to the default sdl surface"
  (sdl:draw-string-solid-* string x y))

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

(defun setup ()
  "This code is run once when (start) is run, it is a preamble to (draw).
   Things that are done here:
     sdl window is created.
     ttf fonts are initialized.
     key repeat rate is set.
     Framerate is set.
     Sprites are loaded into global variables."
  (sdl:window 200 200 :title-caption "Clobber")
  (setf (sdl:frame-rate) 60)
  (sdl:enable-unicode)
  (sdl-ttf:init-ttf)
  (sdl:enable-key-repeat 500 150)
  (initialize-object-lookup-table)
  (initialize-sprite-lookup-table))

(defun draw ()
  "This code is looped repeatedly while main is run. The order of drawing code is important."
  (render-tiles))

(defun on-key-down-event (key)
  "This code is run on when an sdl key-down event is detected."
  (case key
    (:sdl-key-up (move (layer *player*) *player* (x-pos *player*) (- (y-pos *player*) 1)))
    (:sdl-key-down (move (layer *player*) *player* (x-pos *player*) (+ (y-pos *player*) 1)))
    (:sdl-key-left (move (layer *player*) *player* (- (x-pos *player*) 1) (y-pos *player*)))
    (:sdl-key-right (move (layer *player*) *player* (+ (x-pos *player*) 1) (y-pos *player*)))
    (:sdl-key-escape (sdl:push-quit-event))))

(defun on-button-down-event (button)  
  "These function are run when a mouse-button-down event is detected."
  (declare (ignore button))
  (when (sdl:mouse-left-p)))

(defun on-quit-event ()
  "Run this code when the quit-event is detected prior to exiting."
  t)

(defun on-idle-event ()
  "This code is run during the idle-event loop."
  (sdl:clear-display sdl:*black*)
  (draw)
  (sdl:update-display))

(defun start ()
  "This is starts the game."
  (bt:make-thread
   #'(lambda ()
       (sdl:with-init ()
           (setup)
           (sdl:with-events ()
             (:quit-event () (on-quit-event))
             (:key-down-event (:key key) (on-key-down-event key))
             (:mouse-button-down-event (:button button) (on-button-down-event button))
             (:idle () (on-idle-event)))))))
