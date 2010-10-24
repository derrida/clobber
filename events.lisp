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