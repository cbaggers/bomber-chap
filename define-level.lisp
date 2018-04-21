(in-package :bomber-chap)

(defvar *levels* (make-hash-table))

(defparameter *level-key*
  '((#\# wall-tile)
    (#\* block-tile)))

;; hmm could be use daft's scenes for this?

(defun register-level (name string)
  ;; {TODO} remove empty lines
  (setf (gethash name *levels*) string))

(defmacro define-level (name string)
  `(register-level ',name ,string))


(define-level :yay
    "
#####################
## *** *** *** *** ##
## *** *** *** *** ##
## *** *** *** *** ##
## *** *** *** *** ##
## *** *** *** *** ##
## *** *** *** *** ##
## *** *** *** *** ##
## *** *** *** *** ##
#####################")

(defun get-level (name)
  (gethash name *levels*))

(defun spawn-level (name pos)
  (let ((starting-pos (v! pos))
        (level-string (get-level name)))
    (loop :for char :across level-string :do
       (let ((tile (cadr (assoc char *level-key*))))
         (cond
           ((char= char #\space)
            (spawn 'floor-tile pos))
           (tile
            (spawn 'floor-tile pos)
            (spawn tile pos))
           ((char= #\newline char)
            (setf (x pos) (- (x starting-pos) *tile-size*)
                  (y pos) (+ (y pos) *tile-size*)))
           (t (warn "Unknown level symbol ~a" char))))
       (incf (x pos) *tile-size*))))



(defun kill-level-tiles ()
  ;; hack: only for dev
  (kill-all-of 'block-tile)
  (kill-all-of 'wall-tile)
  (kill-all-of 'floor-tile))
