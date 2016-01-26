#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(define size 800)
(define width size)
(define height size)

(define gap (/ height 8))

(define-struct posn (x y))
(define-struct myline (x1 y1 x2 y2))
(define-struct world (c1count c2count c1 c2 lines))

(define initial (make-world
                 0 0
                 (make-posn (/ width 2) (/ height 4))
                 (make-posn (/ width 2) (- (/ height 4) gap))
                 empty))


(define (draw-lines lines baseimg)
  (cond
    ((empty? lines) baseimg)
    (else
      (draw-lines (cdr lines)
                  (add-line baseimg
                            (myline-x1 (car lines))
                            (myline-y1 (car lines))
                            (myline-x2 (car lines))
                            (myline-y2 (car lines))
                            'white)))))

(define (draw w)
  (place-image
   (circle 15 'solid 'blue)
   (posn-x (world-c1 w))
   (posn-y (world-c1 w))
   (place-image
    (circle 15 'solid 'pink)
    (posn-x (world-c2 w))
    (posn-y (world-c2 w))
    (place-image ;; place the sun
     (circle 20 'solid 'yellow)
     (/ width 2)
     (/ height 2)
     (draw-lines (world-lines w)
                 (rectangle width height 'solid 'black))))))

(define (tick w)
  (let ((count1 (world-c1count w))
        (x1 (posn-x (world-c1 w)))
        (y1 (posn-y (world-c1 w)))
        (count2 (world-c2count w))
        (x2 (posn-x (world-c2 w)))
        (y2 (posn-y (world-c2 w))))
  (make-world
   (+ count1 0.08)
   (+ count2 0.13)
   (make-posn (+ (/ width 2) (* (+ gap (/ size 4)) (sin count1))) (+ (/ height 2) (* (+ gap (/ size 4)) (cos count1))))
   (make-posn (+ (/ width 2) (* (/ size 4) (sin count2))) (+ (/ height 2) (* (/ size 4) (cos count2))))
   (if (= x1 x2)
       '()
       (cons (make-myline x1 y1 x2 y2) (world-lines w))))))

(big-bang initial
          (on-tick tick 0.1)
          (to-draw draw))
