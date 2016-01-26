#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require racket/flonum)

(define size 800)
(define width size)
(define height size)

(define gap (/ height 8))

(define-struct posn (x y))
(define-struct myline (x1 y1 x2 y2))
(define-struct world (c1count c2count c1 c2 lines ln))

(define prev-image false)

(define initial (make-world
                 (->fl 0) (->fl 0)
                 (make-posn (->fl (/ width 2)) (->fl (/ height 4)))
                 (make-posn (->fl (/ width 2)) (->fl (- (/ height 4) gap)))
                 empty
                 false))

;; if prev-image is not false, then ln is not false either
(define (place-lines lines baseimg ln)
  (if prev-image
      (let ((added-img 
             (add-line prev-image
                       (myline-x1 ln)
                       (myline-y1 ln)
                       (myline-x2 ln)
                       (myline-y2 ln)
                       'white)))
        (begin
          (set! prev-image added-img)
          added-img))
      (begin
        (set! prev-image (rectangle width height 'solid 'black))
        prev-image)))

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
     (place-lines (world-lines w)
                  (rectangle width height 'solid 'black)
                  (world-ln w))))))
     

(define (findNewCount count)
  (cond
    ((< count (* 2 pi)) count)
    (else (findNewCount (- count (* 2 pi))))))

(define (tick w)
  (let* ((count1 (world-c1count w))
         (x1 (posn-x (world-c1 w)))
         (y1 (posn-y (world-c1 w)))
         (count2 (world-c2count w))
         (x2 (posn-x (world-c2 w)))
         (y2 (posn-y (world-c2 w)))
         ;(fl300 300.0)
         ;(fl400 400.0)
         ;(fl200 200.0)
         (newCount1 (findNewCount count1))
         (newCount2 (findNewCount count2))
         )
  (make-world
   (fl+ count1 0.009)
   (fl+ count2 0.013)
   ;(make-posn (+ (/ width 2) (* (+ gap (/ size 4)) (sin count1))) (+ (/ height 2) (* (+ gap (/ size 4)) (cos count1))))
   ;(make-posn (+ (/ width 2) (* (/ size 4) (sin count2))) (+ (/ height 2) (* (/ size 4) (cos count2))))
   (make-posn (fl+ 400.0 (fl* 300.0 (flsin newCount1))) (fl+ 400.0 (fl* 300.0 (flcos newCount1))))
   (make-posn (fl+ 400.0 (fl* 200.0 (flsin newCount2))) (fl+ 400.0 (fl* 200.0 (flcos newCount2))))
;  
;   (if (= x1 x2)
;       '()
;       (cons (make-myline x1 y1 x2 y2) (world-lines w)))
   empty
   (make-myline x1 y1 x2 y2)
   )))

(big-bang initial
          (on-tick tick 0.01)
          (to-draw draw))
