;; script:  scheme

(define (test x) (+ x 1))

(define-macro (inc! x dx) `(set! ,x (+ ,x ,dx)))

(define t 0)
(define sw 240)
(define sh 136)

(define (TIC)
  (t80::circb (/ sw 2) (/ sh 2) 30 1)
  (let loop ((x 0) (y 0))
    (if (< y sh)
        (begin (if (or (= (modulo t 120) 0)
                       (not (= (t80::pix x y) 0)))
                   (t80::pix x y (floor (random 16))))
               (if (< x sw)
                   (loop (+ x 1) y)
                   (loop 0 (+ y 1))))))
  (inc! t 1))

;; <PALETTE>
;; 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>

