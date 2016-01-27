#lang racket

(require 2htdp/image)

;; Svannah Tree & Sierpinski Triangle 
;;
;; written by Caner Derici
;; November 2010

(define-struct posn (x y))

(define SIZE 400)

;; too-small? : posn posn -> boolean
(define (too-small? p1 p2)
  (< (abs (- (posn-x p1) (posn-x p2))) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; svannah tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; svannah : posn angle(radian) number scene ->  scene
;; draws s svannah tree by drawing a signle straight line 
;; and draws also the branches recursively using circle-pt 
;; to determine the *start* point of the recurring branch
(define (svannah wide start angle size scene)
 (cond
   ((<= size 0.2) scene)
   (else
     (local ((define a (circle-pt angle start (/ size 5)))
             (define b (circle-pt angle start (* 2 (/ size 5))))
             (define c (circle-pt angle start (* 3 (/ size 5))))
             (define d (circle-pt angle start (* 4 (/ size 5))))
             (define main (circle-pt angle start size))
             (define right-angle (+ angle 75))
             (define left-angle (- angle 75))
             (define new-size (* size wide))
             )
       (svannah wide d left-angle new-size
                (svannah wide c right-angle new-size
                         (svannah wide b left-angle new-size
                                  (svannah wide a right-angle new-size
                                           (draw-trunk start main scene)))))))))

;; draw-trunk: posn posn scene -> scene
(define (draw-trunk start end scene)
  (let ((sx (posn-x start))
        (sy (posn-y start))
        (ex (posn-x end))
        (ey (posn-y end)))
  (add-line scene sx sy ex ey (if (< (+ (abs (- sx ex))
                                        (abs (- sy ey))) 10) 
                                  "darkgreen" "brown"))))

;;circle-pt- angle(radian)x posn x number ---> posn
;;makes a posn that is on the circle by multiplying the given
;;angles(factor) cosine&sine by the given radius(r) of the circle and applying 
;;them to the x&y of the given center(center-posn) of circle
(define (circle-pt factor center-posn r)
   (make-posn (+ (posn-x center-posn) (* r (cos factor)))
              (- (posn-y center-posn) (* r (sin factor)))))


;; size is the dimentions of the scene
(define (svannah-main size)
  (svannah 0.4 ;; wideness of the tree
           (make-posn (/ size 2) size)
           (/ pi 2)
           (/ (* size 4) 5)
           (empty-scene size size)))

(svannah-main (* 2 (/ SIZE 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sierpinski triangle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generate-triangle : posn posn posn scene -> image
(define (generate-triangle p1 p2 p3 scene)
  (add-line
   (add-line
    (add-line scene
              (posn-x p1) (posn-y p1) (posn-x p2) (posn-y p2) "red")
    (posn-x p2) (posn-y p2) (posn-x p3) (posn-y p3) "red")
   (posn-x p3) (posn-y p3) (posn-x p1) (posn-y p1) "red"))

(define (mid p1 p2)
  (make-posn (/ (+ (posn-x p1) (posn-x p2)) 2)
             (/ (+ (posn-y p1) (posn-y p2)) 2)))


;; sierpinski : posn posn posn scene -> image
;; think of 
;; p1 as the top vertex
;; p2 as the bottom-right one
;; p3 as the bottom-left
;; usage: see below.
(define (sierpinski p1 p2 p3 scene)
  (cond
    ((too-small? p1 p2) scene)
    (else
     (local ((define p1-p2 (mid p1 p2))
             (define p1-p3 (mid p1 p3))
             (define p2-p3 (mid p2 p3)))
       (generate-triangle p1 p2 p3
                          (sierpinski p1-p2 p2 p2-p3
                                      (sierpinski p1-p3 p2-p3 p3
                                                  (sierpinski p1 p1-p2 p1-p3 scene))))))))


(define (sierpinski-main size)
  (cond
    ((<= size 41) "Heisenberg's uncertainty principle!")
    (else
     (sierpinski (make-posn (/ size 2) 20)
                 (make-posn (- size 20) (- size 20))
                 (make-posn 20 (- size 20))
                 (empty-scene size size)))))
;(sierpinski-main SIZE)