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