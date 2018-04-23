(in-package :bomber-chap)

;;------------------------------------------------------------

(defun run ()
  (when *god*
    (as *god*
      (change-state :start)))
  (daft :start))

(defun stop ()
  (daft :stop))

;;------------------------------------------------------------
