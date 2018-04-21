(in-package #:bomber-chap)

;;------------------------------------------------------------

(setf daft::*system-hack* :bomber-chap)
(setf *screen-height-in-game-units* 1200f0)
(defparameter *tile-size* 64f0)
(defparameter *level-origin* (v! 0 0))
(defparameter *shake* nil)
;;------------------------------------------------------------

(define-god ()
  (:main
   (if *shake*
       (funcall *shake*)
       (setf (focus-offset) (v! 0 0)))))

(defun start-shake (duration magnitude)
  (setf *shake*
        (then
          (before (seconds duration)
            (let ((strength (* magnitude (- 1f0 %progress%))))
              (setf (focus-offset)
                    (v! (* strength (sin (* 100 %progress%)))
                        (* strength (cos (* 60 %progress%)))))))
          (once (setf *shake* nil)))))

(define-actor chap ((:visual "images/bomberman/bman.png")
                    (:tile-count (8 4))
                    (:default-depth 10)
                    (:origin (0 -48))
                    (bombs nil)
                    (simultaneous-bomb-count 10)
                    (cool-down-hack 0))
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
     (focus-camera)
     (decf cool-down-hack)
     (let ((touching-bomb-p (coll-with 'bomb)))
       (setf bombs (remove-if #'is-dead bombs))
       (when (and (pad-button 0)
                  (< (length bombs) simultaneous-bomb-count)
                  (not touching-bomb-p)
                  (<= cool-down-hack 0))
         (print "placed bomb")
         (setf cool-down-hack 3)
         ;; Ok, need a snapped offset
         (let ((place-pos (v! 0 20))
               (snap-pos (snap-position (v! 0 0) *tile-size*)))
           (push (spawn 'bomb place-pos
                        :dest (v2:- snap-pos place-pos))
                 bombs)))))))

(define-actor flame ((:visual "images/flame/flame.png")
                     (:tile-count (5 1))
                     (:default-depth 50)
                     (time-to-die (after (seconds 1) t)))
  (:main
   (advance-frame 0.3)
   (when (funcall time-to-die)
     (die))))

(define-actor bomb ((:visual "images/bomb/bomb.png")
                    (:tile-count (3 1))
                    (:default-depth 40)
                    (dest (v! 0 0))
                    (range 3)
                    (splode (after (seconds 3) t)))
  (:main
   (let ((mv (v2:*s dest 0.1)))
     ;; another place where the local-only thing feels off
     (compass-dir-move mv)
     (v2:decf dest mv))
   (when (funcall splode)
     (loop
        :for i :below range
        :for o := (* i *tile-size*)
        :do
        (spawn 'flame (v! 0 o))
        (spawn 'flame (v! 0 (- o)))
        (spawn 'flame (v! o 0))
        (spawn 'flame (v! (- o) 0)))
     (start-shake 1 10)
     (die))
   (advance-frame 0.1)))

(define-actor wall-tile ((:visual "images/blocks/wall.png")
                         (:default-depth 70))
  (:main))

(define-actor block-tile ((:visual "images/blocks/block.png")
                          (:default-depth 60))
  (:main
   (when (coll-with 'flame)
     (spawn 'dying-block-tile (v! 0 0))
     (die))))

(define-actor dying-block-tile
    ((:visual "images/blocks/block.png")
     (:default-depth 60)
     (angle (+ 1f0 (random 2f0)))
     (anim nil))
  (:setup
   (setf anim
         (then
           (before (seconds 2.5)
             (turn-right angle)
             (compass-angle-move 180 15))
           (once (die))))
   (change-state :run))
  (:run
   (funcall anim)))

(define-actor floor-tile ((:visual "images/blocks/floor.png")
                          (:default-depth 80))
  (:main))

(defun kill-all-of (kind-name)
  ;; hack: only for dev
  (loop :for x :across
     (daft::this-frames-actors
      (daft::get-actor-kind-by-name *current-scene* kind-name))
     :do (as x (die))))

(defun kill-all-bombs ()
  ;; hack: only for dev
  (kill-all-of 'bomb))
