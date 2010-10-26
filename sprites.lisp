(in-package :clobber)

;;; Sprite Functions

(defun add-sprite (name)
  "This function adds a sprite/path pair to the *sprite-lookup-table* hash-table."
  (setf (gethash name *sprite-lookup-table*) (sdl:load-image (asdf:system-relative-pathname :clobber (concatenate 'string "images/" (string-downcase (symbol-name name)) ".png")))))

(defun initialize-sprite-table ()
  "This function initializes the *sprite-lookup-table* with it's starting values."
  (add-sprite 'empty)
  (add-sprite 'player)
  (add-sprite 'earth)
  (add-sprite 'stone)
  (add-sprite 'grass))

(defun lookup-sprite (object)
  (gethash object *sprite-lookup-table*))

