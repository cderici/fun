#lang racket

#|
This ball always want to be in the center of the world!

Just click anywhere.
|#

(require 2htdp/image)
(require 2htdp/universe)

(define s-h 400)
(define s-w 400)

(define ball-rad 20)

(define-struct ball (x y))

(define init-ball (make-ball 5 5))

(define (move-to-center b)
  (let*
      (
       (center-x (/ s-w 2))
       (center-y (/ s-h 2))
       (b-x (ball-x b))
       (b-y (ball-y b))
       )
    (cond 
      [(and (= b-x center-x) (= b-y center-y)) b]
      [(= b-x center-x) (make-ball b-x (+ b-y (/ (- center-y b-y)
                                                 (abs (- center-y b-y)))))]
      [(= b-y center-y) (make-ball (+ b-x (/ (- center-x b-x)
                                             (abs (- center-x b-x)))) b-y)]
      [else
       (let ((new-x (+ b-x (/ (- center-x b-x)
                              (abs (- center-x b-x))))))
         (make-ball
          new-x
          (+ center-y (* (/ (- b-y center-y) (- b-x center-x)) (- new-x center-x)))))])))
  

(define (tick b)
  (move-to-center b))

(define (draw b)
  (place-image (text "Click!" 20 'blue)
               (- s-w 40)
               (- s-h 40)
  (place-image (rectangle (* ball-rad 2) (* ball-rad 2) 'outline 'red)
               (/ s-h 2)
               (/ s-w 2)
               (place-image (circle ball-rad 'solid 'blue)
                            (ball-x b)
                            (ball-y b)
                            (empty-scene s-h s-w)))))

(define (mouse b x y me)
  (cond
    ((mouse=? me "button-down") 
     (begin
       (display x) (display " ") (display y) (newline)
       (make-ball x y)))
    (else b)))

(big-bang init-ball 
          (on-tick tick)
          (to-draw draw)
          (on-mouse mouse))