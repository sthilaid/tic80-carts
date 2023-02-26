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
(define (rescale val min max) (/ (- val min) (- max min)))
(define (lerp ratio min max) (+ min (* ratio (- max min))))
(define (identity x) x)

(define-macro (trace-sexp sexp) `(t80::trace (format #f "~S" ,sexp)))
(define-macro (inc! x dx) `(set! ,x (+ ,x ,dx)))
(define-macro (enum name . enum-values)
  `(begin ,@(mapi (lambda (i v) (let ((sym (symbol-append name "::" v))) `(define-constant ,sym ',sym)))
                  enum-values)))
(define-macro (flags name . flag-values)
  `(begin ,@(mapi (lambda (i v) (let ((sym (symbol-append name "::" v))) `(define-constant ,sym ',i)))
                  flag-values)))

(define debug-break? #f)
(define debug-break-once-list '())
(define (break) (set! debug-break? #t))
(define (break-once id)
  (if (not (memq id debug-break-once-list))
      (begin (set! debug-break-once-list (cons id debug-break-once-list))
             (break))))

(define (swap-palette-color current-color new-color)
  (let ((palette-map-addr #x3FF0))
    (t80::poke4 (+ (* palette-map-addr 2) current-color) new-color)))

(define-macro (with-palette-swaps swaps . body)
  `(begin ,@(map (lambda (color-swap-el) `(swap-palette-color ,(car color-swap-el) ,(cadr color-swap-el)))
                 swaps)
          ,@body
          ,@(map (lambda (color-swap-el) `(swap-palette-color ,(car color-swap-el) ,(car color-swap-el)))
                 swaps)))

;;-----------------------------------------------------------------------------
;; Vec

(defstruct vec (x 0) (y 0))
(define (vec+ v1 val)
  (cond ((vector? val)    (make-vec (+ (vec-x v1) (vec-x val)) (+ (vec-y v1) (vec-y val))))
        ((number? val)  (make-vec (+ (vec-x v1) val) (+ (vec-y v1) val)))
        (#t v1)))
(define (vec-sqrlen v) (+ (expt (vec-x v) 2) (expt (vec-y v) 2)))
(define (vec-round v) (make-vec (round (vec-x v)) (round (vec-y v))))
(define (vec->string v) (format #f "(~1,2F ~1,2F)" (vec-x v) (vec-y v)))

;;-----------------------------------------------------------------------------
;; Player

(enum pstate idle run inair wallslide walljump slope dead)
(enum dir left right)
(defstruct player pos vel dir state (slope-tile #f))
(define (flip-player-dir) (player-set-dir! p (if (eq? (player-dir p) dir::left) dir::right dir::left)))
(define (dir->num dir) (case dir ((dir::left) -1.0) ((dir::right) 1.0)))

;;-----------------------------------------------------------------------------
;; Save Point

(enum spoint-state idle active)
(defstruct savepoint pos (state spoint-state::idle))

;;-----------------------------------------------------------------------------
;; Contants

(define-constant epsilon 0.001)
(define-constant player-cam-x-offset 40)
(define-constant ground-speed 0.75)
(define-constant gravity 0.12)
(define-constant jump-impulse (- 2.2))
(define-constant slope-speed 1.0)
(define-constant screen-w 240)
(define-constant screen-h 136)
(define-constant palette-size (* 16 3))
(define-constant palette-addr #x3FC0)
(define-constant right-slope-tile 3)

(flags sflag collision slope)

;;-----------------------------------------------------------------------------
;; Globals

(define p (make-player (make-vec) (make-vec) dir::right pstate::idle))
(define save-points (list (make-savepoint (make-vec (* 8 9) (* 8 7)))
                          (make-savepoint (make-vec (* 8 63) (* 8 14)))))

(define t 0)

;;-----------------------------------------------------------------------------
;; TIC Callbacks

(define (BOOT)
  (save-palette)
  (restart-level))

(define (TIC)
  (cond
   (debug-break? (if (t80::btnp 4) (set! debug-break? #f)))
   (else
    (update-inputs)
    (update-player)
    (draw)
    (inc! t 1))))
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
            ((check-flag (car pos) flag)
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

(define (check-flag pos flag)
  (let ((tile (t80::mget (quotient (vec-x pos) 8) (quotient (vec-y pos) 8))))
    (if (t80::fget tile flag) tile #f)))

(define (check-flag-under pos flag)
  (let ((x (vec-x pos)) (y (vec-y pos)))
    (let loop ((dx 0) (dy 0))
      (cond ((check-flag (make-vec (+ x dx) (+ y dy)) flag) => identity)
            ((>= dy 8) #f)
            ((< dx 8) (loop (+ dx 1) dy))
            (else (loop 0 (+ dy 1)))))))

(define (intersect-rect? a-min a-max b-min b-max)
  (and (<= (vec-x b-min) (vec-x a-max))
       (<= (vec-y b-min) (vec-y a-max))
       (>= (vec-x b-max) (vec-x a-min))
       (>= (vec-y b-max) (vec-y a-min))))

;; (define (intersect-with-flag minpos maxpos flag)
;;   (let ((min-x (vec-x minpos)) (min-y (vec-y minpos))
;;         (max-x (vec-x maxpos)) (max-y (vec-y maxpos)))
;;     (let loop ((x min-x) (y min-y))
;;       (cond ((check-flag (make-vec x y) flag) #t)
;;             ((> y max-y) #f)
;;             ((> x max-x) (loop min-x (+ y 1)))
;;             (else (loop (+ x 1) y))))))

;;-----------------------------------------------------------------------------
;; init

(define (restart-level)
  (let ((start-pos (let loop ((sp save-points))
                     (cond ((null? sp) (make-vec 12 100))
                           ((eq? (savepoint-state (car sp)) spoint-state::active) (vec+ (savepoint-pos (car sp)) (make-vec 0 (- 10))))
                           (else (loop (cdr sp)))))))
    (vec-set-x! (player-pos p) (vec-x start-pos))
    (vec-set-y! (player-pos p) (vec-y start-pos))
    (change-state pstate::inair)))

;;-----------------------------------------------------------------------------
;; player update

(define (change-state new-state)
  (if (not (eq? new-state (player-state p)))
      (begin (player-set-state! p new-state)
             (trace-sexp `(t: ,t new-state: ,new-state))
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
(define (can-jump?) (let ((pos (player-pos p))
                          (state (player-state p)))
                      (and (or (is-ground-state? state)
                               (eq? state pstate::slope))
                           (not (detect-collision pos
                                                  (vec+ pos (make-vec 0 (- 1)))
                                                  (vec+ pos (make-vec 7 (- 1)))
                                                  sflag::collision)))))
(define (can-wall-jump?)
  (eq? (player-state p) pstate::wallslide))

(define (can-wallslide?)
  (let* ((pos (player-pos p))
         (state (player-state p))
         (wall-detect-pos (vec+ pos (make-vec (if (eq? (player-dir p) dir::left) -2 8) 0))))
    (and (or (is-air-state? state)
             (eq? state pstate::wallslide))
         (detect-collision pos wall-detect-pos wall-detect-pos sflag::collision))))

(define (on-slope?)
  (let* ((pos (player-pos p))
         (vel (player-vel p)))
    (and (>= (vec-y vel) (- epsilon))
         (check-flag-under pos sflag::slope))))

(define (update-player)
  (if (> (vec-y (player-pos p)) screen-h)
      (change-state pstate::dead))
  
  (let ((state (player-state p)))
    (cond ((is-ground-state? state)         (update-ground))
          ((is-air-state? state)            (update-inair))
          ((eq? state pstate::wallslide)    (update-wall))
          ((eq? state pstate::slope)        (update-slope))
          ))
  (update-dynamic-collisions))

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

    (cond ((on-slope?) => (lambda (slope-tile)
                            (player-set-slope-tile! p slope-tile)
                            (change-state pstate::slope))))
    ;;(t80::trace "-- end of update-ground")
    ))

(define (move-xy pos vel flag)
  (let* ((vx-vel (make-vec (vec-x vel) 0))
         (vy-vel (make-vec 0 (vec-y vel)))
         (pos+dx (vec+ pos vx-vel))
         (coll-x (detect-collision pos pos+dx (vec+ pos+dx 7) flag))
         (pos+dx-fixed (get-collision-best-pos coll-x pos pos+dx vx-vel))
         (pos+dxy (vec+ pos+dx-fixed vy-vel))
         (coll-xy (detect-collision pos+dx-fixed pos+dxy (vec+ pos+dxy 7) flag))
         (finalpos (get-collision-best-pos coll-xy pos pos+dxy vy-vel)))
    ;;(trace-sexp `(p: ,pos pdx: ,pos+dx cx: ,coll-x pdxf: ,pos+dx-fixed pdxy: ,pos+dxy cxy: ,coll-xy final: ,finalpos))
    (list finalpos coll-x coll-xy)))

(define (update-slope)
  (let* ((pos (player-pos p))
         (vel (player-vel p))
         (slope-tile (player-slope-tile p)))
    (let* ((slope-dir (cond ((not slope-tile)   (player-dir p))
                            ((= slope-tile      right-slope-tile) dir::right)
                            (else               dir::left)))
           (slope-dir-num (dir->num slope-dir)))
      (vec-set-y! vel slope-speed)
      (vec-set-x! vel (* slope-dir-num (vec-y vel) 1.0)) ; (tan (deg->rad 45.0)) -> 1.0
      (player-set-dir! p slope-dir)
      (let* ((target (vec-round (vec+ pos vel)))
             (is-still-on-slope (check-flag-under target sflag::slope)))
        ;;(trace-sexp `(slope-dir-num ,slope-dir-num pos ,(vec->string pos) vel ,(vec->string vel) target ,(vec->string target)))
        (player-set-slope-tile! p is-still-on-slope)
        (player-set-pos! p target)
        ;; wait 2 frames before transitionning out to avoid some edge cases
        (if (and (not slope-tile) (not is-still-on-slope))
            (change-state pstate::inair))))))

(define (update-wall)
  (let* ((pos (player-pos p))
         (vel (player-vel p))
         (wall-gravity (if (> (vec-y vel) epsilon)
                           (* 0.1 gravity)
                           gravity)))
    (vec-set-y! vel (+ (vec-y vel) wall-gravity))
    ;; kill vx?
    (let* ((move-result (move-xy pos vel sflag::collision))
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
            ((not (can-wallslide?))
             (change-state pstate::inair))
            ((on-slope?) => (lambda (slope-tile)
                              (player-set-slope-tile! p slope-tile)
                              (change-state pstate::slope)))))))

(define (update-inair)
  (let ((pos (player-pos p))
        (vel (player-vel p)))
    (vec-set-y! vel (+ (vec-y vel) gravity))
    (if (and (eq? (player-state p) pstate::walljump)
             (> (vec-y vel) 0))
        (change-state pstate::inair))
    (if (> (abs (vec-x vel)) epsilon)
        (player-set-dir! p (if (> (vec-x vel) epsilon) dir::right dir::left)))
    (let* ((move-result (move-xy pos vel sflag::collision))
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
            ((on-slope?) => (lambda (slope-tile)
                              (player-set-slope-tile! p slope-tile)
                              (change-state pstate::slope)))))))

(define (update-dynamic-collisions)
  (let* ((pos (player-pos p))
         (pmin pos) (pmax (vec+ pos 7)))
      (for-each (lambda (spoint) (let* ((sp-pos (savepoint-pos spoint))
                                        (coll? (intersect-rect? pmin pmax sp-pos (vec+ sp-pos 7))))
                                   (if coll?
                                       (begin
                                         (for-each (lambda (x) (savepoint-set-state! x spoint-state::idle)) save-points)
                                         (savepoint-set-state! spoint spoint-state::active)))))
                save-points))
  )

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

(define (get-animspr period . sprites)
  (let* ((len (length sprites))
         (iperiod (exact->inexact period))
         (spr-index (floor (modulo (/ t iperiod) len))))
    (list-ref sprites spr-index)))
    
(define (draw-player)
  (let* ((state (player-state p))
         (pos (player-pos p))
         (x player-cam-x-offset)
         (y (floor (vec-y pos)))
         (transparent 0)
         (scale 1)
         (dy (if (eq? state pstate::slope) 4 0)))

    (let ((sprite (case state
      ((pstate::idle) (get-animspr 60 259 260))
      ((pstate::run) (get-animspr 5 256 257 258))
      ((pstate::wallslide) 265)
      ((pstate::slope) 266)
      ((pstate::inair pstate::walljump)
       (cond ((< (abs (vec-y (player-vel p))) 0.5)  261)
             ((< (vec-y (player-vel p)) 0)          (get-animspr 5 259 260))
             (else                                  (get-animspr 5 262 263)))))))
      (t80::spr sprite x (+ y dy) transparent scale (if (eq? (player-dir p) dir::left) 1 0)))))

(define (pos->screen pos)
  (let ((minx (- (vec-x (player-pos p)) player-cam-x-offset))
        (miny 0))
    (make-vec (floor (- (vec-x pos) minx)) (floor (- (vec-y pos) miny)))))

(define (draw-save-point spoint)
  (let* ((pos (savepoint-pos spoint))
         (tr 0)
         (scale 1)
         (screen-pos (pos->screen pos))
         (is-visible? (and (>= (vec-x screen-pos) 0)
                           (<= (vec-x screen-pos) 248))))
    (if is-visible?
        (case (savepoint-state spoint)
          ((spoint-state::idle)
           (let ((sprite (get-animspr 45 352 353 354 353)))
             (t80::spr sprite (vec-x screen-pos) (vec-y screen-pos) tr scale)))
          ((spoint-state::active)
           (with-palette-swaps ((14 2) (13 3))
                               (t80::spr 353 (vec-x screen-pos) (vec-y screen-pos) tr scale)))))))

(define (draw-objects)
  (for-each draw-save-point save-points))

(define (draw-level)
  (let* ((px (vec-x (player-pos p)))
         (cam-offset player-cam-x-offset)
         (x (floor (/ (- px cam-offset) 8))) (y 0)
         (w 31) (h 17)
         (sx (- (floor (modulo px 8)))) (sy 0)
         (bg-start 84) (bg-parallax 64)
         (bgx (floor (/ (- px cam-offset) bg-parallax)))
         (bgsx (- (floor (lerp (rescale (modulo (- px cam-offset) bg-parallax) 0 bg-parallax) 0 8))))
         (bg-ratio (rescale (modulo bgx w) 0 w))
         (bgw (floor (* w (- 1.0 bg-ratio))))
         (map-transparent 0) (bg-transparent 2))
    (t80::map bgx bg-start bgw h bgsx sy bg-transparent)
    (t80::map (modulo (+ bgx bgw) w) bg-start (- w bgw) h (+ bgsx (* bgw 8)) sy bg-transparent)
    (t80::map x y w h sx sy map-transparent))
  (draw-objects))

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
;; 001:66666666f6f6f6f6ffdffffffffffffffffdffdffdffffffffffffffffffffdf
;; 002:fffffffffffffdffffdffffffffffffffffdffdffdffffffffffffffffffffdf
;; 003:60000000f6000000f6600000fff60000fffd6000fdfff600fffdff60ffffffd6
;; 004:000000060000006f0000066f00006fff0006dfff006fffdf06ffdfff6dffffff
;; 224:00bbfbbb0bbffbbb0bbbfbbb0bbbfbbb0bbbffbb0bbbbfbb00bbbbbb00bbbbbb
;; 225:bbbbfbb0bbbbfbb0bbbbfbb0bbbfbbb0bbbfbb00bbbbbbb0bbbbbbb0bbbbbbb0
;; 226:7777777777007777770777777777777700707777770b77777777777777777777
;; 227:777777770077777777777777700777077b777777777777777777777777777777
;; 228:7777770077707770777777707777b77777b70007700777007777077077707700
;; 229:00007000070700707077077777770707707777707777b0777777707777777777
;; 240:0bbbbbbb0bbbbbbb00bbbbbb0bbbbbbb0bbbbbbb0bbfbbbb0bbffbbb0bbffbbb
;; 241:bbfbbbb0bffbbb00bffbbbb0bbfbbbb0bbfbbbb0bffbbb00bfbbbb00bbbbbbb0
;; 242:77777777770007777777777777777777777777777777b7777770077777777777
;; 243:bb7770777777bb777777777777777777707777777007770777777b0777777777
;; 244:00770777077077770077700770777077770077770777777707b7b77700777777
;; 245:777777777707777777007777077777b770777b70777077770700070000007000
;; </TILES>

;; <SPRITES>
;; 000:0000088000000890000022200022880002008800000808000080008009000900
;; 001:0000088000000890000022200222880000008800000088000008080000909000
;; 002:0000088000000890000022200022880000008800000088000008800000099000
;; 003:0000880000008900000022000002880000028800002088000000880000009900
;; 004:0000880000008900000022000002880000028800000288000000880000009900
;; 005:0000880000008900022222000000880000008800000088000000880000009900
;; 006:0020880000028900000222000000880000008800000088000000880000009900
;; 007:0002880000028900000222000000880000008800000088000000880000009900
;; 008:0002880000028900000222000000880000008800000088000000880000009900
;; 009:0000880000009808008022080008888000008820000088200000808900009000
;; 010:0008800008089000082228000288808000088800000098800000008800000009
;; 096:00000000000ee00000eede0000eeee00000ee000000000000000000000000000
;; 097:0000000000000000000ee00000edde0000eeee00000ee0000000000000000000
;; 098:000000000000000000000000000ee00000edde0000eede00000ee00000000000
;; </SPRITES>

;; <MAP>
;; 005:000000000000000000000000000010101010101010300000000000000000000000000000000000000000000000000000000000000000000000002020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 006:000000000000000000000000101020000000002020203000000000000000000000000000000000000000000000000000000000000000000000002020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 007:000000000000000000000000000000000000002020202030000000000000000000000000000000000000000000000000000000000000000000002020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 008:100000000000000010101000000000000000002020202020300000000000000000000000000000000000000000000000000000000000000000002020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010101010
;; 009:200000000000001020202000000000000000002020202020203000000000101010101010100000000000000000001010101010101010101010002020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020202020
;; 010:200000000000000000202010101010101000002020202020202030000000000000000000000000000000000000002020000000000000000000002020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020202020
;; 011:200000000000000000000000202020202000002020202020202020300000000000000000000000000000000010002020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020202020
;; 012:200000000000000000000000000000202000002020202020202020203000000000000000000000000000000000002020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020202020
;; 013:200000000000000000000000000000202000002020202020202020202000000000000000000000000000000000002020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020202020
;; 014:200000000000000000000000000000000000002020202020202020202000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020202020
;; 015:200000000000000010000000000000000000002020202020202020202000000000000000101010100000001010101010101010101010000000000000001010101010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020202020
;; 016:201010101010101020101010000010101010102020202020202020202000001010101010202020200000102020202020202020202020000000100000102020202020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020202020
;; 085:004f4e0000004f4e005e4e0000000000004f2f3f5e5e004f5e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 086:4f2f3f5e5e5e2f3f2e2f3f4e000000004f2f3f2e2f3f5e3e3f5e5e4e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 087:4f3f3f3f3f2f3f3f2f3f2f3f5e5e4e4f2f3f5e2f3f2f2f3f2f3f2f4e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 088:4f3e2e3e2e3e2e3e3e2e3e2e2f2f2e3e3f3f3f3f2f3f3f2f3f2f3f5e5e4e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 089:4f3f2f3f2f3f2f3f3f2f3f2f2e2e2f3f3e2e3e2e3e2e3e3e2e3e2e2f2f4e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 090:4f3e2e3e2e3e2e3e2e3e2e2e2f2f4e4f3f2f3f2f3f2f3f3f2f3f2f2e2e4e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 091:4f3f2f3f2f3f5f5f2f3f5f2e2e4e004f3e2e3e2e3e2e3e2e3e2e2e2f2f4e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 092:4f3e2e3e2e4e00000e1e005f5f4e004f3f2f3f2f3f5f5f2f3f5f2e2e4e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 093:4f5f2f3f5f4e00000f1f00000000004f3e2e3e2e4e00000e1e005f5f4e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 094:00000e1e000000000e1e00000000004f5f2f3f5f4e00000f1f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 095:00000f1f000000000f1f000000000000000e1e000000000e1e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 096:00000e1e000000000e1e000000000000000f1f000000000f1f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 097:00000f1f000000000f1f000000000000000e1e000000000e1e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 098:00000e1e000000000e1e000000000000000f1f000000000f1f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 099:00000f1f000000000f1f000000000000000e1e000000000e1e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 100:00000e1e000000000e1e000000000000000f1f000000000f1f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 101:00000f1f000000000f1f000000000000000e1e000000000e1e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
;; 000:00101020200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 001:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008080800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </FLAGS>

;; <PALETTE>
;; 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76414713429366f3b5dc941a6f63424142c55ee94b0c2566c86333c57
;; </PALETTE>

