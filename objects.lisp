(in-package :clobber)

;;; Sprite Functions

(defun add-sprite (name)
  "This function adds a sprite/path pair to the *sprite-lookup-table* hash-table."
  (setf (gethash name (sprites *world*))
        (sdl:load-image (asdf:system-relative-pathname
                         :clobber
                         (concatenate 'string
                                      "images/"
                                      (string-downcase (symbol-name name))
                                      ".png")))))

(defun initialize-sprite-table ()
  "This function initializes the *sprite-lookup-table* with it's starting values."
  (add-sprite 'empty)
  (add-sprite 'player)
  (add-sprite 'earth)
  (add-sprite 'stone)
  (add-sprite 'grass))

(defun lookup-sprite (object)
  (gethash object (sprites *world*)))


;;   (defun add-object (name)
;;       (setf (gethash counter *object-lookup-table*) name)
;;       (defobject name () :inherit (unit))
;;       (incf counter)))

(let ((counter 0))
  (defun add-object (name)  
    (defobject name () :inherit (unit))
    (setf (gethash counter (objects *world*)) name)
    ;(push (make-instance name) (objects *world*))
    (incf counter)))

;;  `(add-sprite ,name) ;;  add-sprite is where see load-image get run


;;(let ((new-sym (intern (concatenate 'string "*" name "*")))))
;; `(if (boundp ,new-sym)
;;          t
;;          (defparameter ,new-sym ))

(defun initialize-object-table ()
  (add-object 'earth)
  (add-object 'stone)
  (add-object 'water)
  (add-object 'dirt)
  (add-object 'grass)
  (add-object 'fire)
  (add-object 'clay)
  (add-object 'air))

(defun lookup-object (id)
  (gethash id (objects *world*)))

;; (defobject earth () :inherit (unit)) ; 0
;; (defobject stone () :inherit (unit)) ; 1
;; (defobject water () :inherit (unit)) ; 2
;; (defobject dirt  () :inherit (unit)) ; 3
;; (defobject fire  () :inherit (unit)) ; 4
;; (defobject clay  () :inherit (unit)) ; 5
;; (defobject air   () :inherit (unit)) ; 6

;;; Object Functions
