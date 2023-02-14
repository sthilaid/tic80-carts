;; title:   game title
;; author:  game developer, email, etc.
;; desc:    short description
;; site:    website link
;; license: MIT License (change this to your license of choice)
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

(define (detect-collision min max flag)
  (let ((xmin (vec-x min)) (xmax (vec-x max)) (ymin (vec-y min)) (ymax (vec-y max)))
    (let loop ((x xmin) (y ymin))
      (cond ((> y ymax) #f)
            ((let* ((mx (floor (/ x 8)))
                    (my (floor (/ y 8)))
                    (tile (t80::mget mx my))
                    (coll? (t80::fget tile flag)))
               (trace-sexp `(x ,x y ,y mx ,mx my ,my tile ,tile coll ,coll?))
               coll?)
             (if (= x xmin) (make-vec x (- y 8)) (make-vec (- x 8) y)))
            ((< x xmax) (loop (+ x 1) y))
            (else (loop xmin (+ y 1)))))))

(define (update-ground)
  (t80::trace (format #f "~S" `(update-ground)))
  (let ((pos (player-pos p))
        (vel (player-vel p)))
    (cond ((> (abs (vec-x vel)) 0)
           (begin (player-set-state! p pstate::run)
                  (let* ((nextpos (vec+ pos (make-vec (vec-x vel) 0.0)))
                         (coll (detect-collision nextpos (vec+ nextpos 7) collision-flag)))
                    (vec-set-x! pos (vec-x (if coll coll nextpos))))
                  (player-set-dir! p (if (< (vec-x vel) 0) dir::left dir::right))))
          (#t
           (player-set-state! p pstate::idle)))

    (t80::trace "-- before end of update-ground")
    (let ((groundpos (vec+ pos (make-vec 0 1))))
      (if (not (detect-collision groundpos (vec+ groundpos 7) collision-flag))
          (player-set-state! p pstate::inair)
          (vec-set-y! vel 0.0)))
    (t80::trace "-- end of update-ground")
    ))

(define (update-inair)
  (let ((pos (player-pos p))
        (vel (player-vel p)))
    (vec-set-y! vel (+ (vec-y vel) gravity))
    (let* ((nextpos (vec+ pos vel))
           (coll? (detect-collision nextpos (vec+ nextpos 7) collision-flag)))
      (if coll?
          (begin (player-set-pos! p coll?)
                 (if (> (vec-y vel) 0)
                     (player-set-state! p pstate::idle)))
          (player-set-pos! p nextpos)))))

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
  (t80::trace (format #f "~S" `(draw)))
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

