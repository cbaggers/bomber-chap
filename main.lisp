(in-package :bomber-chap)

;;------------------------------------------------------------

(defun run ()
  (as *god*
    (change-state :start))
  (daft :start))

(defun stop ()
  (daft :stop))

;;------------------------------------------------------------
