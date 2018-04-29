(in-package :bomber-chap)

;;------------------------------------------------------------

(def-shipping-manifest :bomber-chap run
  :compression nil
  :libs-to-include (cl-soil::soil
                    (sdl2-mixer::libsdl2-mixer :recur)
                    (sdl2::libsdl2 :recur))
  "images/"
  "audio/")

(setf daft::*system-hack* :bomber-chap)
(set-local-path-function
 (lambda (sys path) (shipshape:local-path path sys)))

;;------------------------------------------------------------

(defun run ()
  (when *god*
    (as *god*
      (change-state :start)))
  (daft :start))

(defun stop ()
  (daft :stop))

;;------------------------------------------------------------
