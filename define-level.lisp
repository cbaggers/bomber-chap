(in-package :bomber-chap)

(defvar *levels* (make-hash-table))
(defvar *current-level* nil)

(defparameter *level-key*
  '((#\# wall-tile)
    (#\* block-tile)))

;; hmm could be use daft's scenes for this?

(defun register-level (name string)
  ;; {TODO} remove empty lines
  (setf (gethash name *levels*) string))

(defmacro define-level (name string)
  `(progn
     (register-level ',name ,string)
     ;; hack
     (push (lambda ()
             (when (eq *current-level* ',name)
               (as *god*
                 (kill-level-tiles)
                 (spawn-level ',name))))
           daft::*tasks-for-next-frame*)))


(define-level :yay
    "
#####################
## *** *** *** *** ##
## *** *** *** *** ##
## *** *** *** *** ##
## ***     *** *** ##
## *** *** *** *** ##
## *** *** *** *** ##
## *** ***     *** ##
## *** *** *** *** ##
#####################")

(defun get-level (name)
  (gethash name *levels*))

(defun spawn-level (name)
  (let* ((starting-pos *level-origin*)
         (pos (v! starting-pos))
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
       (incf (x pos) *tile-size*))
    (setf *current-level* name)))



(defun kill-level-tiles ()
  ;; hack: only for dev
  (kill-all-of 'block-tile)
  (kill-all-of 'wall-tile)
  (kill-all-of 'floor-tile))
