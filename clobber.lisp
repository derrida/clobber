(in-package :clobber)

;;; Object Functions

(let ((counter 0))
  (defun add-object (name)
      (setf (gethash counter *object-lookup-table*) name)
      (incf counter)))

(defun initialize-object-table ()
  (add-object 'empty)
  (add-object 'player)
  (add-object 'earth)
  (add-object 'stone)
  (add-object 'grass))

(defun lookup-object (id)
  (gethash id *object-lookup-table*))

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

;;; Print Functions
(defun print-hash-entry (key value)
  (format t "The value associated with the key ~S is ~S~%" key value))

(defun print-hash-table (table)
  (maphash #'print-hash-entry table))

(defun print-object-table ()
  (print-hash-table *object-lookup-table*))

(defun print-sprite-table ()
  (print-hash-table *sprite-lookup-table*))

;;; Render Functions

(defun render-tiles ()
  "This function renders  tiles across the default SDL window."
  (loop for i from 0 to 9 do
       (loop for j from 0 to 9 do
            (let ((val (layer-value-at (layer-at-player) i j)))
              (render (lookup-sprite (lookup-object (value-at i j))) (* i 8) (* j 8))
              (when (> val 0)
                  (render (lookup-sprite (lookup-object val)) (* i 8) (* j 8)))))))