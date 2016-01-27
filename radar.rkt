#lang racket

(require 2htdp/image)
(require 2htdp/universe)

; world : ang

(define angle-step 5)

(define r 100)

(define (draw w)
  (scene+line (empty-scene 200 200)
              (+ 100 (* r (cos (degrees->radians w))))
              (- 100 (* r (sin (degrees->radians w))))
              100
              100
              'red))

(define (tick w)
  (+ w angle-step))

(big-bang 0 (on-tick tick) (to-draw draw))

