(in-package :clobber)

;;; Render Functions

;(defun sprite-lookup (id)
;  id)

(defun render-tiles ()
  "This function renders  tiles across the default SDL window."
  (loop for i from 0 to 9 do
       (loop for j from 0 to 9 do
            (let* ((val (layer-value-at (layer-at-player) i j))
                    (obj (lookup-object (value-at i j)))
                    (sprite (lookup-sprite obj)))
              ;; (format t "~A~%~A~%~A~%" val obj sprite) ;; Test render-tiles by commenting the render line.
               (render sprite (* i 8) (* j 8))))))

;(when (> val 0)
;  (print (render (lookup-sprite val) (* i 8) ,(* j 8))))

;;(lookup-sprite (lookup-object val)) ))))))
;;               (lookup-sprite (lookup-object (value-at i j)))