(in-package #:bomber-chap)

;;------------------------------------------------------------

(setf daft::*system-hack* :bomber-chap)
(setf *screen-height-in-game-units* 1200f0)
(defparameter *tile-size* 64f0)

;;------------------------------------------------------------

(define-actor chap ((:visual "images/bomberman/bman.png")
                    (:tile-count (8 4))
                    (bombs nil)
                    (simultaneous-bomb-count 10))
  (:main
   (let ((ang (compass-angle-from-analog 0)))
     (when ang
       ;; not sure this feels nice though
       (let* ((dir-floor (floor (/ (+ ang 45) 90f0)))
              (clamped-ang (* dir-floor 90f0))
              (dir-id (mod (+ 2 dir-floor) 4)))
         (compass-angle-move clamped-ang 5f0)
         (case dir-id
           ;; could just split id into list but then am
           ;; consing lists all the time
           (0 (advance-frame 0.4 '(9 16)))
           (1 (advance-frame 0.4 '(17 24)))
           (2 (advance-frame 0.4 '(0 8)))
           (3 (advance-frame 0.4 '(25 32))))))
     (let ((touching-bomb-p (coll-with 'bomb)))
       (setf bombs (remove-if #'is-dead bombs))
       (when (and (pad-button 0)
                  (< (length bombs) simultaneous-bomb-count)
                  (not touching-bomb-p))
         (print "placed bomb")
         (push (spawn 'bomb (v! 0 0)) bombs))))))

(define-actor flame ((:visual "images/flame/flame.png")
                     (:tile-count (5 1))
                     (time-to-die (after (seconds 1) t)))
  (:main
   (advance-frame 0.3)
   (when (funcall time-to-die)
     (die))))

(define-actor bomb ((:visual "images/bomb/bomb.png")
                    (:tile-count (3 1))
                    (range 3)
                    (splode (after (seconds 3) t)))
  (:main
   (when (funcall splode)
     (loop
        :for i :below range
        :for o := (* i *tile-size*)
        :do
        (spawn 'flame (v! 0 o))
        (spawn 'flame (v! 0 (- o)))
        (spawn 'flame (v! o 0))
        (spawn 'flame (v! (- o) 0)))
     (die))
   (advance-frame 0.1)))

(defun kill-all-bombs ()
  ;; hack: only for dev
  (loop :for x :across
     (daft::this-frames-actors
      (daft::get-actor-kind-by-name *current-scene* 'bomb))
     :do (as x (die))))
