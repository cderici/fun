#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(define height 400)
(define width 600)

(define radius 65)
(define speed 0.1)

(define factor 0.35)

(define center1-x (* 2/3 width))
(define center2-x (* 1/3 width))

(define center1-y (* 1/2 height))
(define center2-y (* 1/2 height))


(struct world (eye1 eye2))

(struct eye (pos center angle))

(struct posn (x y))

(define (tick w)
  (let*
      ((eye1 (world-eye1 w))
       (eye2 (world-eye2 w))
       (center1 (eye-center eye1))
       (center2 (eye-center eye2))
       (angle1 (eye-angle eye1))
       (angle2 (eye-angle eye2)))
  (world (eye (posn (+ (posn-x center1) (* radius (cos angle1)))
                    (- (posn-y center2) (* factor radius (sin angle1))))
              center1
              ;angle1)
              (begin
                (display angle1) (newline)
                (+ speed angle1)))
         (eye (posn (+ (posn-x center2) (* radius (cos angle2)))
                    (- (posn-y center2) (* factor radius (sin angle2))))
              center2
              ;angle2)
              (- angle2 speed))
         )
    )
  )

(define (tick2 w) w)


(define eye-img
  (overlay/xy
   (circle 10 'solid 'white)
   -25 -10
   (circle 25 'solid 'black)))

(define (draw w)
  (place-image
   eye-img
   (posn-x (eye-pos (world-eye1 w)))
   (posn-y (eye-pos (world-eye1 w)))
   (place-image
    eye-img
    (posn-x (eye-pos (world-eye2 w)))
    (posn-y (eye-pos (world-eye2 w)))
    (place-image
     (ellipse 200 100 'outline 'black)
     (* 2/3 width) 
     (/ height 2)
     (place-image
      (ellipse 200 100 'outline 'black)
      (* 1/3 width) 
      (/ height 2)
      (empty-scene width height))))))

(define (mouse w x y me)
  (cond
    ((mouse=? me "move")
     (world
      (eye (eye-pos (world-eye1 w))
           (eye-center (world-eye1 w))
           (- 1 (atan (/ (- y center1-y) (if (zero? (- center1-x x)) 0.01
                                             (- center1-x x)))))
           )
      (eye (eye-pos (world-eye2 w))
           (eye-center (world-eye2 w))
           (- 1 (atan (/ (- y center2-y) (if (zero? (- center2-x x)) 0.001
                                             (- center2-x x)))))
           ))
     )
    (else
     w)))



(big-bang (world (eye (posn (+ center1-x radius) center1-y)
                      (posn center1-x center1-y)
                      0)
                 (eye (posn (+ center2-x radius) center2-y)
                      (posn center2-x center2-y)
                      0))
          (on-tick tick) (to-draw draw) );(on-mouse mouse))