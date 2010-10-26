(in-package :clobber)

;;; Object Definitions

(defobject unit (x y hp inventory))

;; (defobject earth () :inherit (unit)) ; 0
;; (defobject stone () :inherit (unit)) ; 1
;; (defobject water () :inherit (unit)) ; 2
;; (defobject dirt  () :inherit (unit)) ; 3
;; (defobject fire  () :inherit (unit)) ; 4
;; (defobject clay  () :inherit (unit)) ; 5
;; (defobject air   () :inherit (unit)) ; 6

;;; Object Functions
;; (let ((counter 0))
;;   (defun add-object (name)
;;       (setf (gethash counter *object-lookup-table*) name)
;;       (defobject name () :inherit (unit))
;;       (incf counter)))

(defmacro add-object (name)
  `(defobject ,name () :inherit (unit)))

(defun initialize-object-table ()
  (add-object earth)
  (add-object stone)
  (add-object water)
  (add-object dirt)
  (add-object grass)
  (add-object fire)
  (add-object clay)
  (add-object air))

(defun lookup-object (id)
  (gethash id *object-lookup-table*))