(in-package :bomber-chap)

;;------------------------------------------------------------

(defvar *levels* (make-hash-table))
(defvar *current-level* nil)

;; hmm could be use daft's scenes for this?

(defun register-level (name string)
  ;; {TODO} remove empty lines
  (setf (gethash name *levels*) string))

(defmacro define-level (name string)
  `(progn
     (register-level ',name ,string)
     (push (lambda ()
             (change-level ',name))
           daft::*tasks-for-next-frame*)))

(defun level-tile-dims (str)
  (let ((max 0)
        (wip 0)
        (lines 0))
    (loop :for char :across str :do
       (if (char= char #\newline)
           (progn
             (incf lines)
             (setf max (max wip max))
             (setf wip 0))
           (incf wip)))
    (values max lines)))


;;------------------------------------------------------------

(defun get-level (name)
  (gethash name *levels*))

(defun change-level (name)
  ;; hack
  (as *god*
    (kill-level-tiles)
    (spawn-level name)))

(defun tile-spawn (kind pos &rest args)
  ;; We use this so it's easier to debug
  ;; levels that dont fit in the world
  (when (and (< (abs (x pos)) 1024)
             (< (abs (y pos)) 1024))
    (apply #'spawn kind pos args)))

(defvar *resize-body* (lambda ()))

(defun resized ()
  (when *resize-body*
    (funcall *resize-body*)))

(defun spawn-level (name)
  (let ((level-string (get-level name))
        (starting-pos (v! *level-origin*)))
    (multiple-value-bind (width height)
        (level-tile-dims level-string)
      (v2:incf starting-pos
               (v! (- (* (floor (* width 0.5)) *tile-size*))
                   (* (floor (* height 0.5)) *tile-size*)))
      (let* ((pos (v! starting-pos))
             (lines 0))
        (loop :for char :across level-string :do
           (cond
             ((char= char #\#)
              (spawn 'floor-tile pos)
              (tile-spawn 'wall-tile pos))
             ((char= char #\*)
              (spawn 'floor-tile pos)
              (tile-spawn 'block-tile pos))
             ((char= char #\0)
              (spawn 'floor-tile pos)
              (tile-spawn 'chap-0 (v2:- pos (v! 0 20))
                          :spawn-point (tile-spawn 'spawn-point pos)))
             ((char= char #\1)
              (spawn 'floor-tile pos)
              (tile-spawn 'chap-1 (v2:- pos (v! 0 20))
                          :spawn-point (tile-spawn 'spawn-point pos)))
             ((char= char #\space)
              (spawn 'floor-tile pos))
             ((char= #\. char)
              (spawn 'floor-tile pos)
              (tile-spawn (alexandria:random-elt '(flame-powerup
                                                   bomb-powerup
                                                   bomb-powerup
                                                   bomb-powerup
                                                   speed-powerup
                                                   speed-powerup
                                                   speed-powerup))
                          pos)
              (tile-spawn 'block-tile pos))
             ((char= #\newline char)
              (incf lines)
              (setf (x pos) (- (x starting-pos) *tile-size*)
                    (y pos) (- (y pos) *tile-size*)))
             (t (warn "Unknown level symbol ~a" char)))
           (incf (x pos) *tile-size*))
        (flet ((resized ()
                 (let ((res (surface-resolution (current-surface))))
                   (setf *screen-height-in-game-units*
                         (+ (* (/ (* (max width height) *tile-size*) (x res))
                               (y res))
                            (* 5 *tile-size*))))))
          (resized)
          (setf *resize-body* #'resized))
        (add-window-resize-listener 'resized)
        (setf *current-level* name)))))

(defun kill-level-tiles ()
  ;; hack: only for dev
  (kill-all-of 'ghost)
  (kill-all-of 'chap-0)
  (kill-all-of 'chap-1)
  (kill-all-of 'block-tile)
  (kill-all-of 'wall-tile)
  (kill-all-of 'floor-tile)
  (kill-all-of 'flame-powerup)
  (kill-all-of 'bomb-powerup)
  (kill-all-of 'speed-powerup))

;;------------------------------------------------------------
