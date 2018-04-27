(in-package #:bomber-chap)

;;------------------------------------------------------------

(setf daft::*system-hack* :bomber-chap)
(setf *screen-height-in-game-units* 1200f0)
(defparameter *tile-size* 64f0)
(defparameter *level-origin* (v! 0 0))
(defparameter *shake* nil)

(defvar *round-win* 3)
(defvar *player-0-wins* 0)
(defvar *player-1-wins* 0)

(defun reset-wins ()
  (setf *player-0-wins* 0
        *player-1-wins* 0)
  t)

;;------------------------------------------------------------

(define-audio
  (:all :channels 32))

(define-god ((logo nil t)
             (level-button-down nil t))
  (:start
   (load-all-levels)
   (add-timer :kick 0.49)
   (play-track "audio/city-stomper.ogg")
   (sdl2-mixer:volume-music 50)
   (change-state :load-menu))
  (:load-menu
   (change-level :menu)
   (setf logo (spawn 'logo (v! 0 0)))
   (change-state :menu))
  (:menu
   (when (or (key-down-p key.return)
             (pad-button 0))
     (kill logo)
     (setf logo nil)
     (next-level)
     (change-state :game)))
  (:game
   (when (time-p :kick)
     (start-shake 0.1 3))
   (setf level-button-down (check-next-level level-button-down))
   (if *shake*
       (funcall *shake*)
       (setf (focus-offset) (v! 0 0)))))

(defun check-next-level (level-button-down)
  (if (key-down-p key.j)
      (when (not level-button-down)
        (next-level)
        (setf level-button-down t))
      (setf level-button-down nil))
  level-button-down)

(defun start-shake (duration magnitude)
  (setf *shake*
        (then
          (before (seconds duration)
            (let ((strength (* magnitude (- 1f0 %progress%))))
              (setf (focus-offset)
                    (v! (* strength (sin (* 100 %progress%)))
                        (* strength (cos (* 60 %progress%)))))))
          (once (setf *shake* nil)))))

(define-actor logo ((:visual "images/menu/title.png")
                    (:default-depth 5))
  (:main))

;;------------------------------------------------------------
;; As our collision mask give us so little info, and this is
;; only 2 player, we are bodging how we handle who owns what

(defun move-chap (ang speed)
  (let* ((dir-floor (floor (/ (+ ang 45) 90f0)))
         (clamped-ang (* dir-floor 90f0))
         (dir-id (mod (+ 2 dir-floor) 4)))
    (compass-angle-move clamped-ang speed)
    (case dir-id
      ;; could just split id into list but then am
      ;; consing lists all the time
      (0 (advance-frame 0.4 '(9 16)))
      (1 (advance-frame 0.4 '(17 24)))
      (2 (advance-frame 0.4 '(0 8)))
      (3 (advance-frame 0.4 '(25 32))))
    (compass-angle-dir clamped-ang speed)))

(defun ang-for-chap (id)
  (flet ((keyb (up down left right)
           (cond
             ((key-down-p up) 0f0)
             ((key-down-p down) 180f0)
             ((key-down-p left) 90f0)
             ((key-down-p right) 270f0))))
    (if (= id 0)
        (or (compass-angle-from-analog 0 0.1 (skitter:gamepad id))
            (keyb key.w key.s key.a key.d))
        (keyb key.up key.down key.left key.right))))

(defun drop-for-chap (id)
  (if (= id 0)
      (or (pad-button 0 (gamepad id)) (key-down-p key.tab))
      (key-down-p key.space)))

;; this is bad :D
(macrolet
    ((define-chap-badness (id)
       (let ((name (intern (format nil "CHAP-~a" id)))
             (other-wins-var (elt '(*player-1-wins* *player-0-wins*) id)))
         `(define-actor ,name ((:visual "images/bomberman/bman.png")
                               (:tile-count (8 4))
                               (:collision-mask "images/bomberman/cmask.png")
                               (:default-depth 20)
                               (:origin (0 -48))
                               (last-dir (v! 0 0))
                               (bombs nil)
                               (simultaneous-bomb-count 1)
                               (cool-down-hack 0)
                               (splode-size 1)
                               (speed 3f0)
                               (spawn-point nil t)
                               (invincible-time (before (seconds 3) t)))
            (:main
             (let* ((ang (ang-for-chap ,id)))
               (when ang
                 ;;
                 ;; blech, need proper collision info
                 ;; without some semblence of that it's hard to bodge sliding
                 (when (or (coll-with 'wall-tile)
                           (coll-with 'block-tile)
                           (coll-with ',(elt '(bomb-1 bomb-0) id)))
                   (compass-dir-move (v2:negate last-dir)))
                 ;;
                 ;; not sure this feels nice though
                 (setf last-dir (move-chap ang speed)))
               (decf cool-down-hack)
               (let ((touching-bomb-p (or (coll-with 'bomb-0)
                                          (coll-with 'bomb-1))))
                 (setf bombs (remove-if #'is-dead bombs))
                 (when (and (drop-for-chap ,id)
                            (< (length bombs) simultaneous-bomb-count)
                            (not touching-bomb-p)
                            (<= cool-down-hack 0))
                   (setf cool-down-hack 3)
                   ;; Ok, need a snapped offset
                   (let ((place-pos (v! 0 20))
                         (snap-pos (snap-position (v! 0 0) *tile-size*)))
                     (push (spawn ',(elt '(bomb-0 bomb-1) id)
                                  place-pos
                                  :dest (v2:- snap-pos place-pos)
                                  :range splode-size)
                           bombs)))))
             (when (coll-with 'speed-powerup)
               (incf speed 1f0))
             (when (coll-with 'flame-powerup)
               (incf splode-size 1))
             (when (coll-with 'bomb-powerup)
               (incf simultaneous-bomb-count 1))
             (when (and (coll-with 'flame)
                        (not (funcall invincible-time)))
               (incf ,other-wins-var)
               (die)
               (if (>= ,other-wins-var *round-win*)
                   (progn
                     (spawn 'dying-chap (v! 0 0))
                     (as *god*
                       (spawn ',(elt '(chap-1-wins chap-0-wins) id)
                              (v! 0 0))))
                   (spawn 'ghost (v! 0 0)
                          :of ',name
                          :spawn-point spawn-point))))))))

  (define-chap-badness 0)
  (define-chap-badness 1))

(define-actor dying-chap
    ((:visual "images/bomberman/bman.png")
     (:tile-count (8 4))
     (:default-depth 20)
     (anim (then
             (before (seconds 2.5)
               (turn-right 20)
               (compass-angle-move 180 15)))
           t))
  (:spin
   (funcall anim)))

(defun make-win-do ()
  (then
    (before (seconds 3)
      nil)
    (once
     (next-level)
     (die))))

(define-actor chap-0-wins ((:visual "images/p0win.png")
                           (:default-depth 3)
                           (do (make-win-do)))
  (:main
   (funcall do)))

(define-actor chap-1-wins ((:visual "images/p1win.png")
                           (:default-depth 3)
                           (do (make-win-do)))
  (:main
   (funcall do)))

;;------------------------------------------------------------

(define-actor ghost ((:visual "images/bomberman/ghost.png")
                     (:default-depth 10)
                     (:origin (0 -48))
                     (of nil t)
                     (spawn-point nil t))
  (:main
   (if spawn-point
       (let ((dist (distance-to spawn-point)))
         (move-towards spawn-point (* 0.1 dist))
         (when (< dist 4)
           (die)
           (as spawn-point
             (spawn of (v! 0 0)
                    :spawn-point spawn-point))))
       (progn
         (die)
         (warn "No respawn point for ~a" of)))))

;;------------------------------------------------------------

(define-actor flame ((:visual "images/flame/flame.png")
                     (:tile-count (5 1))
                     (:default-depth 50)
                     (time-to-die (after (seconds 0.7) t)))
  (:main
   (advance-frame 0.3)
   (when (funcall time-to-die)
     (die))))

;;------------------------------------------------------------
;; As our collision mask give us so little info, and this is
;; only 2 player, we are bodging how we handle who owns what

(progn
  (defun bomb-common (dest range splode sound)
    (let ((mv (v2:*s dest 0.1)))
      ;; another place where the local-only thing feels off
      (compass-dir-move mv)
      (v2:decf dest mv))
    (when (funcall splode)
      (loop
         :for i :below (1+ range)
         :for o := (* i *tile-size*)
         :do
         (spawn 'flame (v! 0 o))
         (spawn 'flame (v! 0 (- o)))
         (spawn 'flame (v! o 0))
         (spawn 'flame (v! (- o) 0)))
      (start-shake 1 10)
      (play-sound :all sound)
      (die))
    (advance-frame 0.1))

  (define-actor bomb-0 ((:visual "images/bomb/bomb.png")
                        (:tile-count (3 1))
                        (:default-depth 40)
                        (dest (v! 0 0))
                        (range 3)
                        (splode (after (seconds 3) t))
                        (sound (load-audio "audio/explosion-0.wav")))
    (:main (bomb-common dest range splode sound)))

  (define-actor bomb-1 ((:visual "images/bomb/bomb.png")
                        (:tile-count (3 1))
                        (:default-depth 40)
                        (dest (v! 0 0))
                        (range 3)
                        (splode (after (seconds 3) t))
                        (sound (load-audio "audio/explosion-0.wav")))
    (:main (bomb-common dest range splode sound))))

;;------------------------------------------------------------

(define-actor bomb-powerup
    ((:visual "images/powerups/bomb-powerup.png")
     (:default-depth 65))
  (:main
   (when (or (coll-with 'chap-0) (coll-with 'chap-1))
     (die))))

(define-actor flame-powerup
    ((:visual "images/powerups/flame-powerup.png")
     (:default-depth 65))
  (:main
   (when (or (coll-with 'chap-0) (coll-with 'chap-1))
     (die))))

(define-actor speed-powerup
    ((:visual "images/powerups/speed-powerup.png")
     (:default-depth 65))
  (:main
   (when (or (coll-with 'chap-0) (coll-with 'chap-1))
     (die))))

;;------------------------------------------------------------

(define-actor spawn-point ())

;;------------------------------------------------------------

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
                          (:default-depth 80)))

(defun kill-all-of (kind-name)
  ;; hack: only for dev
  (loop :for x :across
     (daft::this-frames-actors
      (daft::get-actor-kind-by-name *current-scene* kind-name))
     :do (as x (die))))
