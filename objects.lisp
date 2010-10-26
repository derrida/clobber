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