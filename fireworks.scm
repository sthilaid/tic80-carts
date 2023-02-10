;; title:   fireworks
;; author:  David St-Hilaire
;; desc:    Simple fireworks in scheme
;; license: MIT License
;; version: 0.1
;; script:  scheme

;;-----------------------------------------------------------------------------
;; constants and global variables

(define gravity 0.05)
(define flash-duration 12)
(define part-gid 0)
(define particles '())
(define flashes '())
(define next-firework 0)

;;-----------------------------------------------------------------------------
;; utils

(define-macro (inc! x dx) `(begin (set! ,x (+ ,x ,dx)) ,x))
(define-macro (docount count . loopbody)
  (let ((loopsym (gensym))
        (isym (gensym)))
    `(let ,loopsym ((,isym 0))
          (if (< ,isym ,count)
              (begin ,@loopbody
                     (,loopsym (+ ,isym 1)))))))

(define (filter pred ls)
  (if (null? ls)
      '()
      (if (pred (car ls))
          (cons (car ls) (filter pred (cdr ls)))
          (filter pred (cdr ls)))))

(define (lerp from to ratio)
  (+ from (* ratio (- to from))))

;;-----------------------------------------------------------------------------
;; particles

(defstruct vec x y)
(defstruct part pos vel col t id)

(define (update-part p)
  (part-set-t! p (- (part-t p) 1))
  (let ((x (+ (vec-x (part-pos p))
	          (vec-x (part-vel p))))
		(y (+ (vec-y (part-pos p))
			  (vec-y (part-vel p)))))
	(part-set-pos! p (make-vec x y)))
  (let ((vx (vec-x (part-vel p)))
	    (vy (+ (vec-y (part-vel p)) gravity)))
	(part-set-vel! p (make-vec vx vy))))

(define (draw-part p)
  (let ((pos (part-pos p)))
    (t80::circ (floor (vec-x pos)) (floor (vec-y pos)) 0 (part-col p))))

;;-----------------------------------------------------------------------------
;; flashes

(defstruct flash pos col t)

(define (update-flash f)
  (flash-set-t! f (- (flash-t f) 1)))

(define (draw-flash f)
  (let* ((pos (flash-pos f))
         (col (flash-col f))
         (t (flash-t f))
         (half-max-t (/ (exact->inexact flash-duration) 2.0))
         (r (lerp 3 5 (if (> t half-max-t) (- 1.0 (/ (- t half-max-t) half-max-t)) (/ t half-max-t)))))
    (t80::circ (vec-x pos) (vec-y pos) (floor r) col)))


;;-----------------------------------------------------------------------------
;; fireworks

(define (fireworks)
  (let ((dv 1.0))
    (docount 1
             (let* ((cx (random 240))
                    (cy (random 136))
                    (col (random 16)))
               (docount 30
                        (let* ((pos (make-vec cx cy))
                               (vel (make-vec (- (random dv) (/ dv 2)) 
                                              (- (random dv))))
                               (t (+ 60 (random 100)))
                               (p (make-part pos vel col t (inc! part-gid 1))))
                          (set! particles (cons p particles))
                          (set! flashes (cons (make-flash (make-vec cx cy) col flash-duration) flashes))))))))

;;-----------------------------------------------------------------------------
;; tic80 callbacks

(define (BOOT)
  (fireworks))

(define (TIC)
  (if (t80::btnp 4) (fireworks))

  (if (= next-firework 0)
      (begin (fireworks)
             (set! next-firework (random (logior 30 1))))
      (inc! next-firework -1))

  (for-each (lambda (p) (update-part p))
            particles)
  (for-each (lambda (f) (update-flash f))
            flashes)
  
  (set! particles (filter (lambda (p) (> (part-t p) 0))
                          particles))
  (set! flashes (filter (lambda (f) (> (flash-t f) 0))
                        flashes))

  (t80::cls 0)
  (for-each (lambda (f) (draw-flash f))
            flashes)
  (for-each (lambda (p) (draw-part p))
            particles)

  ;;(t80::print (format #f "counts: p/~d f/~d" (length particles) (length flashes)) 10 10 15)
  )

;; <TILES>
;; 001:eccccccccc888888caaaaaaaca888888cacccccccacc0ccccacc0ccccacc0ccc
;; 002:ccccceee8888cceeaaaa0cee888a0ceeccca0ccc0cca0c0c0cca0c0c0cca0c0c
;; 003:eccccccccc888888caaaaaaaca888888cacccccccacccccccacc0ccccacc0ccc
;; 004:ccccceee8888cceeaaaa0cee888a0ceeccca0cccccca0c0c0cca0c0c0cca0c0c
;; 017:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
;; 018:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
;; 019:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
;; 020:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
;; </TILES>

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

;; <PALETTE>
;; 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>

