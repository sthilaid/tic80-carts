;; title:   platformer demo
;; author:  sthilaid
;; license: MIT License
;; version: 0.1
;; script:  scheme

;;-----------------------------------------------------------------------------
;; Utils

(define (mapi f lst) (let loop ((lst lst) (i 0)) (if (null? lst) '() (cons (f i (car lst)) (loop (cdr lst) (+ i 1))))))

(define-macro (trace-sexp sexp) `(t80::trace (format #f "~S" ,sexp)))
(define-macro (inc! x dx) `(set! ,x (+ ,x ,dx)))
(define-macro (enum name . enum-values)
  `(begin ,@(mapi (lambda (i v) `(define ,(string->symbol (string-append (symbol->string name) "::" (symbol->string v))) ,i))
                  enum-values)))

;;-----------------------------------------------------------------------------
;; Vec

(defstruct vec (x 0) (y 0))
(define (vec+ v1 val)
  (cond ((vector? val)    (make-vec (+ (vec-x v1) (vec-x val)) (+ (vec-y v1) (vec-y val))))
        ((number? val)  (make-vec (+ (vec-x v1) val) (+ (vec-y v1) val)))
        (#t v1)))
(define (vec-sqrlen v) (+ (expt (vec-x v) 2) (expt (vec-y v) 2)))
(define (vec->string v) (format #f "(~1,2F ~1,2F)" (vec-x v) (vec-y v)))

;;-----------------------------------------------------------------------------
;; Player

(enum pstate idle run inair)
(enum dir left right)
(defstruct player pos vel dir state)

;;-----------------------------------------------------------------------------
;; Contants

(define ground-speed 0.75)
(define gravity 0.12)
(define jump-impulse (- 2.2))
(define collision-flag 0)

;;-----------------------------------------------------------------------------
;; Globals

(define p (make-player (make-vec) (make-vec) dir::right pstate::idle))
(define t 0)

;;-----------------------------------------------------------------------------
;; TIC Callbacks

(define (BOOT)
  (vec-set-x! (player-pos p) 10)
  (vec-set-y! (player-pos p) 10))

(define (TIC)
  (update-inputs)
  (update-player)
  (draw)
  (inc! t 1)
)
;;-----------------------------------------------------------------------------
;; Inputs

(define (update-inputs)
  ;;(if (t80::btn 0) (inc! y -1))
  ;;(if (t80::btn 1) (inc! y 1))
  (if (t80::btn 3) 
      (vec-set-x! (player-vel p) ground-speed)
      (if (t80::btn 2) 
          (vec-set-x! (player-vel p) (- ground-speed))
          (vec-set-x! (player-vel p) 0)))

  (if (and (can-jummp?) (t80::btnp 4))
      (let ((v (player-vel p)))
        (vec-set-y! v (+ (vec-y v) jump-impulse))
        (player-set-state! p pstate::inair))))

;;-----------------------------------------------------------------------------
;; collision detection

(define (detect-collision origin minpos maxpos flag)
  (let ((xmin (vec-x minpos)) (xmax (vec-x maxpos)) (ymin (vec-y minpos)) (ymax (vec-y maxpos)))
    (let loop ((pos (list (make-vec xmin ymin) (make-vec xmin ymax)
                          (make-vec xmax ymin) (make-vec xmax ymax))))
      (cond ((null? pos) #f)
            ((let* ((x (vec-x (car pos)))
                    (y (vec-y (car pos)))
                    (mx (floor (/ x 8)))
                    (my (floor (/ y 8)))
                    (tile (t80::mget mx my))
                    (coll? (t80::fget tile flag)))
               ;;(trace-sexp `(x ,x y ,y mx ,mx my ,my tile ,tile coll ,coll?))
               coll?)
             (let* ((x (vec-x (car pos))) (y (vec-y (car pos)))
                    (ox (vec-x origin))   (oy (vec-y origin))
                    (proper-x (if (< x ox) (+ x (- 8 (remainder x 8))) (- x (remainder x 8))))
                    (proper-y (if (< y oy) (- y (remainder y 8)) (- y (remainder y 8)))))
               (make-vec proper-x proper-y)))
            (else (loop (cdr pos)))))))

(define (get-collision-best-pos coll pos nextpos vel)
  (if (not coll)
      nextpos
      (let* ((vx (vec-x vel))
             (vy (vec-y vel))
             (x (cond ((< (abs vx) 0.0001)  (vec-x pos))
                      ((< vx 0)             (min (vec-x pos) (+ (vec-x coll) 1)))
                      (else                 (max (vec-x pos) (- (vec-x coll) 8)))))
             (y (cond ((< (abs vy) 0.0001)  (vec-y pos))
                      ((< vy 0)             (min (vec-y pos) (+ (vec-y coll) 1)))
                      (else                 (max (vec-y pos) (- (vec-y coll) 8))))))
        (make-vec x y))))


;;-----------------------------------------------------------------------------
;; player update

(define (is-ground-state? state)
  (or (eq? state pstate::idle)
      (eq? state pstate::run)))

(define (is-air-state? state)
  (eq? state pstate::inair))

(define (can-jummp?) (is-ground-state? (player-state p)))

(define (update-player)
  (let ((state (player-state p)))
    (cond ((is-ground-state? state)    (update-ground))
          ((is-air-state? state)       (update-inair)))))

(define (update-ground)
  ;;(t80::trace (format #f "~S" `(update-ground)))
  (let ((pos (player-pos p))
        (vel (player-vel p)))
    (cond ((> (abs (vec-x vel)) 0)
           (begin (player-set-state! p pstate::run)
                  (let* ((nextpos (vec+ pos (make-vec (vec-x vel) 0.0)))
                         (coll? (detect-collision pos nextpos (vec+ nextpos 7) collision-flag)))
                    ;;(trace-sexp `(pos: ,pos ,coll? ,(get-collision-best-pos coll? pos nextpos vel)))
                    (cond ((not coll?)      (vec-set-x! pos (vec-x nextpos)))
                          (else             (vec-set-x! pos (vec-x (get-collision-best-pos coll? pos nextpos vel))))))
                  (player-set-dir! p (if (< (vec-x vel) 0) dir::left dir::right))))
          (#t
           (player-set-state! p pstate::idle)))

    ;;(t80::trace "-- before end of update-ground")
    (let ((groundpos (vec+ pos (make-vec 0 1))))
      (if (not (detect-collision pos groundpos (vec+ groundpos 7) collision-flag))
          (player-set-state! p pstate::inair)
          (vec-set-y! vel 0.0)))
    ;;(t80::trace "-- end of update-ground")
    ))

(define (update-inair)
  (let ((pos (player-pos p))
        (vel (player-vel p)))
    (vec-set-y! vel (+ (vec-y vel) gravity))
    (let* ((vx-vel (make-vec (vec-x vel) 0))
           (vy-vel (make-vec 0 (vec-y vel)))
           (pos+dx (vec+ pos vx-vel))
           (coll-x (detect-collision pos pos+dx (vec+ pos+dx 7) collision-flag))
           (pos+dx-fixed (get-collision-best-pos coll-x pos pos+dx vx-vel))
           (pos+dxy (vec+ pos+dx-fixed vy-vel))
           (coll-xy (detect-collision pos+dx-fixed pos+dxy (vec+ pos+dxy 7) collision-flag))
           (finalpos (get-collision-best-pos coll-xy pos pos+dxy vy-vel)))
      ;;(trace-sexp `(p: ,pos pdx: ,pos+dx cx: ,coll-x pdxf: ,pos+dx-fixed pdxy: ,pos+dxy cxy: ,coll-xy final: ,finalpos))
      (player-set-pos! p finalpos)
      (if (and (> (vec-y vel) 0)
             (or coll-x coll-xy))
        (player-set-state! p pstate::idle)))))

;;-----------------------------------------------------------------------------
;; Draw

(define (get-animspr start end f-period)
  (let ((iperiod (exact->inexact f-period)))
    (+ start (floor (modulo (/ t iperiod) (- end start))))))
    
(define (draw-player)
  (let* ((state (player-state p))
         (pos (player-pos p))
         (x (floor (vec-x pos)))
         (y (floor (vec-y pos)))
         (transparent 0)
         (scale 1)) 
    (cond ((= state pstate::idle)
           (t80::spr (get-animspr 4 6 60) x y transparent scale (if (eq? (player-dir p) dir::left) 1 0)))
          ((= state pstate::run)
           (t80::spr (get-animspr 1 4 5) x y transparent scale (if (< (vec-x (player-vel p)) 0) 1 0)))
          ((= state pstate::inair)
           (t80::spr (get-animspr 4 6 5) x y transparent scale (if (< (vec-x (player-vel p)) 0) 1 0)))
           )))

(define (draw-level)
  (let ((x 0) (y 0)
        (w 30) (h 17)
        (sx 0) (sy 0))
    (t80::map x y w h sx sy)))

(define (draw)
  ;;(t80::trace (format #f "~S" `(draw)))
  (t80::cls 0)
  (draw-level)
  (draw-player)
  (t80::print (format #f "pos: ~A v: ~A s: ~S" (vec->string (player-pos p)) (vec->string (player-vel p)) (player-state p) ))
  )

;;-----------------------------------------------------------------------------
;; Data

;; <TILES>
;; 001:0000088000000890000022200022880002008800000808000080008009000900
;; 002:0000088000000890000022200222880000008800000088000008080000909000
;; 003:0000088000000890000022200022880000008800000088000008800000099000
;; 004:0000880000008900000022000002880000028800002088000000880000009900
;; 005:0000880000008900000022000002880000028800000288000000880000009900
;; 048:66666666f6f6f6f6ffdffffffffffffffffdffdffdffffffffffffffffffffdf
;; 049:fffffffffffffdffffdffffffffffffffffdffdffdffffffffffffffffffffdf
;; </TILES>

;; <MAP>
;; 014:000000000000000000000003030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 015:000000000000000300000013130000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 016:030303030303031303030313130303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </MAP>

;; <WAVES>
;; 000:00000000ffffffff00000000ffffffff
;; 001:0123456789abcdeffedcba9876543210
;; 002:0123456789abcdef0123456789abcdef
;; </WAVES>

;; <SFX>
;; 000:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000304000000000
;; </SFX>

;; <TRACKS>
;; 000:100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </TRACKS>

;; <FLAGS>
;; 000:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </FLAGS>

;; <PALETTE>
;; 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>

