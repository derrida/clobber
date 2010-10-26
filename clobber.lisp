(in-package :clobber)

;;; Print Functions
(defun print-hash-entry (key value)
  (format t "The value associated with the key ~S is ~S~%" key value))

(defun print-hash-key (key value)
  (declare (ignore value))
  (format t "~S~%" key))

(defun print-hash-value (key value)
  (declare (ignore key))
  (format t "~S~%" value))

(defun print-hash-table (table)
  (maphash #'print-hash-entry table))

(defun print-hash-table-keys (table)
  (maphash #'print-hash-key table))

(defun print-hash-table-values (table)
  (maphash #'print-hash-value table))

(defun print-object-table ()
  (print-hash-table *object-lookup-table*))

(defun print-sprite-table ()
  (print-hash-table *sprite-lookup-table*))

(defun print-object-table-keys ()
  (print-hash-table-keys *object-lookup-table*))

(defun print-object-table-values ()
  (print-hash-table-values *object-lookup-table*))

;;; Render Functions

(defun render-tiles ()
  "This function renders  tiles across the default SDL window."
  (loop for i from 0 to 9 do
       (loop for j from 0 to 9 do
            (let ((val (layer-value-at (layer-at-player) i j)))
              (render (lookup-sprite (lookup-object (value-at i j))) (* i 8) (* j 8))
              (when (> val 0)
                  (render (lookup-sprite (lookup-object val)) (* i 8) (* j 8)))))))