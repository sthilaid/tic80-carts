;; title:   shinobit
;; author:  sthilaid
;; license: MIT License
;; version: 0.1
;; script:  scheme

;;-----------------------------------------------------------------------------
;; Utils

(define (symbol-append . syms) (string->symbol (apply string-append (map (lambda (x) (if (symbol? x) (symbol->string x) x))
                                                                         syms))))
(define (mapi f lst) (let loop ((lst lst) (i 0)) (if (null? lst) '() (cons (f i (car lst)) (loop (cdr lst) (+ i 1))))))
(define (deg->rad deg-angle) (/ (* deg-angle pi) 180.0))
(define (rad->deg rad-angle) (/ (* rad-angle 180.0) pi))

(define-macro (trace-sexp sexp) `(t80::trace (format #f "~S" ,sexp)))
(define-macro (inc! x dx) `(set! ,x (+ ,x ,dx)))
(define-macro (enum name . enum-values)
  `(begin ,@(mapi (lambda (i v) (let ((sym (symbol-append name "::" v))) `(define-constant ,sym ',sym)))
                  enum-values)))
(define-macro (flags name . flag-values)
  `(begin ,@(mapi (lambda (i v) (let ((sym (symbol-append name "::" v))) `(define-constant ,sym ',i)))
                  flag-values)))

;;-----------------------------------------------------------------------------
;; Vec

(defstruct vec (x 0) (y 0))
(define (vec+ v1 val)
  (cond ((vector? val)    (make-vec (+ (vec-x v1) (vec-x val)) (+ (vec-y v1) (
vec-y val))))
        ((number? val)  (make-vec (+ (vec-x v1) val) (+ (vec-y v1) val)))
        (#t v1)))
(define (vec-sqrlen v) (+ (expt (vec-x v) 2) (expt (vec-y v) 2)))
(define (vec->string v) (format #f "(~1,2F ~1,2F)" (vec-x v) (vec-y v)))

;;-----------------------------------------------------------------------------
;; Player

(enum pstate idle run inair wallslide walljump slope dead)
(enum dir left right)
(defstruct player pos vel dir state)
(define (flip-player-dir) (player-set-dir! p (if (eq? (player-dir p) dir::left) dir::right dir::left)))
(define (dir->num dir) (case dir ((dir::left) -1.0) ((dir::right) 1.0)))

;;-----------------------------------------------------------------------------
;; Contants

(define-constant epsilon 0.001)
(define-constant player-cam-x-offset 40)
(define-constant ground-speed 0.75)
(define-constant gravity 0.12)
(define-constant jump-impulse (- 2.2))
(define-constant screen-w 240)
(define-constant screen-h 136)
(define-constant palette-size (* 16 3))
(define-constant palette-addr #x3FC0)
(define-constant right-slope-tile 51)

(flags sflag collision slope)

;;-----------------------------------------------------------------------------
;; Globals

(define p (make-player (make-vec) (make-vec) dir::right pstate::idle))
(define t 0)

;;-----------------------------------------------------------------------------
;; TIC Callbacks

(define (BOOT)
  (save-palette)
  (restart-level))

(define (TIC)
  (update-inputs)
  (update-player)
  (draw)
  (inc! t 1)
)
;;-----------------------------------------------------------------------------
;; Inputs

(define (update-inputs)
  (if (and (is-dead?) (or (t80::btnp 4) (t80::btnp 5)))
      (restart-level))
  (if (can-steer?)
      (if (t80::btn 3) 
          (vec-set-x! (player-vel p) ground-speed)
          (if (t80::btn 2) 
              (vec-set-x! (player-vel p) (- ground-speed))
              (vec-set-x! (player-vel p) 0))))

  (if (t80::btnp 4)
      (cond ((can-jump?) (let ((v (player-vel p)))
                            (vec-set-y! v jump-impulse)
                            (change-state pstate::inair)))
            ((can-wall-jump?) (let ((v (player-vel p)))
                                (vec-set-x! v (* (dir->num (player-dir p)) jump-impulse 0.5))
                                (vec-set-y! v jump-impulse)
                                (flip-player-dir)
                                (change-state pstate::walljump))))))

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
               ;; (if (and (eq? (player-state p) pstate::inair)
               ;;          (eq? flag sflag::slope))
               ;;     (trace-sexp `(xy (,x ,y) mxy (,mx ,my) tile ,tile coll ,coll?)))
               coll?)
             (let* ((x (vec-x (car pos))) (y (vec-y (car pos)))
                    (ox (vec-x origin))   (oy (vec-y origin))
                    (proper-x (if (< x ox) (+ x (- 8 (remainder x 8))) (- x (remainder x 8))))
                    (proper-y (if (< y oy) (- y (- 8 (remainder y 8))) (- y (remainder y 8)))))
               (make-vec proper-x proper-y)))
            (else (loop (cdr pos)))))))

(define (get-collision-best-pos coll pos nextpos vel)
  (if (not coll)
      nextpos
      (let* ((vx (vec-x vel))
             (vy (vec-y vel))
             (x (cond ((< (abs vx) epsilon) (vec-x pos))
                      ((< vx 0)             (min (vec-x pos) (round (+ (vec-x coll) 1))))
                      (else                 (max (vec-x pos) (round (- (vec-x coll) 8))))))
             (y (cond ((< (abs vy) epsilon) (vec-y pos))
                      ((< vy 0)             (min (vec-y pos) (round (+ (vec-y coll) 2)))) ;; not sure why in -y we need an extra buffer
                      (else                 (max (vec-y pos) (round (- (vec-y coll) 8)))))))
        ;;(trace-sexp `(best ,coll ,pos ,nextpos ,x ,y))
        (make-vec x y))))

(define (intersect-with-flag minpos maxpos flag)
  (let ((min-x (vec-x minpos)) (min-y (vec-y minpos))
        (max-x (vec-x maxpos)) (max-y (vec-y maxpos)))
   (let loop ((x min-x) (y min-y))
     (let* ((tile-x (quotient x 8)) (tile-y (quotient y 8))
            (tile (t80::mget tile-x tile-y)))
      (cond ((t80::fget tile flag) tile)
            ((> y max-y) #f)
            ((> x max-x) (loop min-x (+ y 1)))
            (else (loop (+ x 1) y)))))))

;;-----------------------------------------------------------------------------
;; init

(define (restart-level)
  (vec-set-x! (player-pos p) 10)
  (vec-set-y! (player-pos p) 10)
  (change-state pstate::idle))

;;-----------------------------------------------------------------------------
;; player update

(define (change-state new-state)
  (if (not (eq? new-state (player-state p)))
      (begin (player-set-state! p new-state)
             ;;(trace-sexp `(t: ,t new-state: ,new-state))
             )))

(define (is-ground-state? state)
  (or (eq? state pstate::idle)
      (eq? state pstate::run)))

(define (is-air-state? state)
  (case state
    ((pstate::inair pstate::walljump) #t)
    (else #f)))

(define (is-dead?)
  (eq? (player-state p) pstate::dead))

(define (can-steer?) (case (player-state p)
                       ((pstate::idle pstate::run pstate::inair pstate::wallslide) #t)
                       (else #f)))
(define (can-jump?) (let ((state (player-state p)))
                      (or (is-ground-state? state)
                          (eq? state pstate::slope))))
(define (can-wall-jump?)
  (eq? (player-state p) pstate::wallslide))

(define (can-wallslide?)
  (let* ((pos (player-pos p))
         (wall-detect-pos (vec+ pos (make-vec (if (eq? (player-dir p) dir::left) -2 8) 0))))
    (and (is-air-state? (player-state p))
         (detect-collision pos wall-detect-pos wall-detect-pos sflag::collision))))

(define (should-slide?)
  (let* ((pos (player-pos p))
         (vel (player-vel p)))
    (and (>= (vec-y vel) (- epsilon))
         (intersect-with-flag pos (vec+ pos (make-vec 7 9)) sflag::slope))))

(define (update-player)
  (if (> (vec-y (player-pos p)) screen-h)
      (change-state pstate::dead))
  
  (let ((state (player-state p)))
    (cond ((is-ground-state? state)         (update-ground))
          ((is-air-state? state)            (update-inair))
          ((eq? state pstate::wallslide)    (update-wall))
          ((eq? state pstate::slope)        (update-slope))
          )))

(define (update-ground)
  ;;(t80::trace (format #f "~S" `(update-ground)))
  (let ((pos (player-pos p))
        (vel (player-vel p)))
    (cond ((> (abs (vec-x vel)) 0)
           (begin (change-state pstate::run)
                  (let* ((nextpos (vec+ pos (make-vec (vec-x vel) 0.0)))
                         (coll? (detect-collision pos nextpos (vec+ nextpos 7) sflag::collision)))
                    ;;(trace-sexp `(pos: ,pos ,coll? ,(get-collision-best-pos coll? pos nextpos vel)))
                    (cond ((not coll?)      (vec-set-x! pos (vec-x nextpos)))
                          (else             (vec-set-x! pos (vec-x (get-collision-best-pos coll? pos nextpos vel))))))
                  (player-set-dir! p (if (< (vec-x vel) 0) dir::left dir::right))))
          (#t
           (change-state pstate::idle)))

    ;;(t80::trace "-- before end of update-ground")
    (let ((groundpos (vec+ pos (make-vec 0 1))))
      (if (not (detect-collision pos groundpos (vec+ groundpos 7) sflag::collision))
          (change-state pstate::inair)
          (vec-set-y! vel 0.0)))

    (if (should-slide?)
        (change-state pstate::slope))
    ;;(t80::trace "-- end of update-ground")
    ))

(define (move-xy pos vel)
  (let* ((vx-vel (make-vec (vec-x vel) 0))
         (vy-vel (make-vec 0 (vec-y vel)))
         (pos+dx (vec+ pos vx-vel))
         (coll-x (detect-collision pos pos+dx (vec+ pos+dx 7) sflag::collision))
         (pos+dx-fixed (get-collision-best-pos coll-x pos pos+dx vx-vel))
         (pos+dxy (vec+ pos+dx-fixed vy-vel))
         (coll-xy (detect-collision pos+dx-fixed pos+dxy (vec+ pos+dxy 7) sflag::collision))
         (finalpos (get-collision-best-pos coll-xy pos pos+dxy vy-vel)))
    ;;(trace-sexp `(p: ,pos pdx: ,pos+dx cx: ,coll-x pdxf: ,pos+dx-fixed pdxy: ,pos+dxy cxy: ,coll-xy final: ,finalpos))
    (list finalpos coll-x coll-xy)))

(define (update-slope)
  (let* ((pos (player-pos p))
         (vel (player-vel p))
         (slope-tile (intersect-with-flag pos (vec+ pos 8) sflag::slope)))
    (if (not slope-tile)
        (change-state pstate::inair)
        (let* ((slope-dir (if (= slope-tile right-slope-tile) dir::right dir::left))
               (slope-dir-num (dir->num slope-dir))
               (slope-gravity (* gravity 0.01)))
          (vec-set-y! vel (min 1 (+ (vec-y vel) slope-gravity)))
          (vec-set-x! vel (* slope-dir-num (vec-y vel) 1.0)) ; (tan (deg->rad 45.0)) -> 1.0
          (player-set-dir! p slope-dir)
          (let* ((move-result (move-xy pos vel))
                 (finalpos (list-ref move-result 0))
                 (coll-x (list-ref move-result 1))
                 (coll-y (list-ref move-result 2)))
            (player-set-pos! p finalpos)
            (if (and coll-y (not (intersect-with-flag finalpos (vec+ finalpos 7) sflag::slope)))
                (change-state pstate::idle)))))))

(define (update-wall)
  (let* ((pos (player-pos p))
         (vel (player-vel p))
         (wall-gravity (if (> (vec-y vel) epsilon)
                           (* 0.1 gravity)
                           gravity)))
    (vec-set-y! vel (+ (vec-y vel) wall-gravity))
    ;; kill vx?
    (let* ((move-result (move-xy pos vel))
           (finalpos (list-ref move-result 0))
           (coll-x (list-ref move-result 1))
           (coll-y (list-ref move-result 2)))
      ;;(trace-sexp `(p: ,pos pdx: ,pos+dx cx: ,coll-x pdxf: ,pos+dx-fixed pdxy: ,pos+dxy cxy: ,coll-xy final: ,finalpos))
      (player-set-pos! p finalpos)
      (cond ((and coll-y (> (vec-y vel) epsilon))
             (change-state pstate::idle))
            ((and coll-y (< (vec-y vel) epsilon))
             (vec-set-y! vel 0))
            ((and (not coll-x) (> (abs (vec-x vel)) epsilon))
             (change-state pstate::inair))
            ((should-slide?)
             (change-state pstate::slope))))))

(define (update-inair)
  (let ((pos (player-pos p))
        (vel (player-vel p)))
    (vec-set-y! vel (+ (vec-y vel) gravity))
    (if (and (eq? (player-state p) pstate::walljump)
             (> (vec-y vel) 0))
        (change-state pstate::inair))
    (if (> (abs (vec-x vel)) epsilon)
        (player-set-dir! p (if (> (vec-x vel) epsilon) dir::right dir::left)))
    (let* ((move-result (move-xy pos vel))
           (finalpos (list-ref move-result 0))
           (coll-x (list-ref move-result 1))
           (coll-y (list-ref move-result 2)))
      (player-set-pos! p finalpos)
      (cond ((and coll-y (> (vec-y vel) epsilon))
             (change-state pstate::idle))
            ((and coll-y (< (vec-y vel) epsilon))
             (vec-set-y! vel 0))
            ((can-wallslide?)
             (change-state pstate::wallslide))
            ((should-slide?)
             (change-state pstate::slope))))))

;;-----------------------------------------------------------------------------
;; Palette utils

(define (save-palette)
  (t80::memcpy #x15000 palette-addr palette-size))

(define (restore-palette)
  (t80::memcpy palette-addr #x15000 palette-size))

(define (darken-palette)
  (let color-loop ((col 0))
    (if (< col 16)
        (begin (let rgb-loop ((i 0))
                 (if (< i 3)
                     (let* ((col-component (+ palette-addr (* col 3) i))
                            (val (t80::peek col-component)))
                       (t80::poke col-component (logand #x0F val))
                       (rgb-loop (+ i 1)))))
               (color-loop (+ col 1))))))

(define (set-palette-color i r g b)
  (t80::poke (+ palette-addr (* i 3) 0) r)
  (t80::poke (+ palette-addr (* i 3) 1) g)
  (t80::poke (+ palette-addr (* i 3) 2) b))

;;-----------------------------------------------------------------------------
;; Draw

(define (get-animspr start end f-period)
  (let ((iperiod (exact->inexact f-period)))
    (+ start (floor (modulo (/ t iperiod) (- end start))))))
    
(define (draw-player)
  (let* ((state (player-state p))
         (pos (player-pos p))
         ;;(x (floor (vec-x pos)))
         (x player-cam-x-offset)
         (y (floor (vec-y pos)))
         (transparent 0)
         (scale 1))

    (let ((sprite (case state
      ((pstate::idle) (get-animspr 4 6 60))
      ((pstate::run) (get-animspr 1 4 5))
      ((pstate::wallslide) (get-animspr 9 10 5))
      ((pstate::slope) (get-animspr 10 11 5))
      ((pstate::inair pstate::walljump)
       (cond ((< (abs (vec-y (player-vel p))) 0.5)  6)
             ((< (vec-y (player-vel p)) 0)          (get-animspr 4 6 5))
             (else                                  (get-animspr 7 9 5)))))))
      (t80::spr sprite x y transparent scale (if (eq? (player-dir p) dir::left) 1 0)))))

(define (draw-level)
  (let* ((px (vec-x (player-pos p)))
         (x (floor (/ (- px player-cam-x-offset) 8))) (y 0)
         (w 31) (h 17)
         (sx (- (floor (modulo px 8)))) (sy 0)
         (transparent 0))
    (t80::map x y w h sx sy transparent)))

(define (draw)
  ;;(t80::trace (format #f "~S" `(draw)))
  (t80::cls 0)

  (cond ((eq? (player-state p) pstate::dead)
         (darken-palette)
         (set-palette-color 2 #xbe #x31 #x53)
         (draw-level)
         (t80::print "YOU DIED" (- (quotient screen-w 2) 60) (quotient screen-h 2) 2 #t 2))
        (else
         (restore-palette)
         (draw-level)
         (draw-player)))
  (t80::print (format #f "pos: ~A v: ~A" (vec->string (player-pos p)) (vec->string (player-vel p))))
  (t80::print (format #f "state: ~s" (player-state p)) 0 10)
  )

;;-----------------------------------------------------------------------------
;; Data

;; <TILES>
;; 001:0000088000000890000022200022880002008800000808000080008009000900
;; 002:0000088000000890000022200222880000008800000088000008080000909000
;; 003:0000088000000890000022200022880000008800000088000008800000099000
;; 004:0000880000008900000022000002880000028800002088000000880000009900
;; 005:0000880000008900000022000002880000028800000288000000880000009900
;; 006:0000880000008900022222000000880000008800000088000000880000009900
;; 007:0020880000028900000222000000880000008800000088000000880000009900
;; 008:0002880000028900000222000000880000008800000088000000880000009900
;; 009:0000880000009808008022080008888000008820000088200000808900009000
;; 010:0008800008089000082228000288808000088800000098800000008800000009
;; 048:66666666f6f6f6f6ffdffffffffffffffffdffdffdffffffffffffffffffffdf
;; 049:fffffffffffffdffffdffffffffffffffffdffdffdffffffffffffffffffffdf
;; 050:bbbbbbbbbbfbbbbbbffbbbbbbbbbbbbbbbbbbfbbbbbbbfbbbbffbbbbbbbbbbbb
;; 051:60000000f6000000f6600000fff60000fffd6000fdfff600fffdff60ffffffd6
;; 052:000000060000006f0000066f00006fff0006dfff006fffdf06ffdfff6dffffff
;; </TILES>

;; <MAP>
;; 005:000000000000000000000000000000030303030300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 006:330000000000000000000000000303132323231303330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 007:133300000000000000000000000023232323231313003300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 008:001333000000000303030300000023232323231313130033000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 009:000013330000001313131300000023232323231313131300330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 010:000000133300000023131303030303030323231313131313003300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 011:000000001333000023232323131313131323231313131313130033000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 012:000000000013330023232323232323131323231313131313131300330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 013:000000000000133323232323232323131323231313131313131313000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 014:000000000000000023232323232323232323231313131313131313130000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 015:000000000000000323232323232323232323231313131313131313130000000000000003030303000000030303030303030303030300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 016:030303030303031303030300000303030303031313131313131313130000000303030313131313000000131313131313131313131303030303030303000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
;; 000:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010100020200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </FLAGS>

;; <PALETTE>
;; 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f63424142c55ee94b0c2566c86333c57
;; </PALETTE>

