#|
A little funny gravitation simulation

Caner Derici
before 2006
|#

#lang racket

(require 2htdp/image)
(require 2htdp/universe)

;; Data Definitions
;;
;; world : struct number
;; coor : is the structure which contains the x&y coordinates
;; vel-y : is the vel-y
(define-struct world (x y vel-y vel-x start end treshold))
;;
;; coordinate : number number 
;; x : x coordinate
;; y : y coordinate
;;(define-struct coordinate (x y))

;; place-ball world -> scene
;; in an empty 200x200 scene
;; 
;; (place-ball 0) -> (place-image
;;                     (circle 10 'solid 'red)
;;                     100 0 (empty-scene 200 200))
;; 
;; (define (place-ball h)
;;   (... h ...)
(define (place-ball n)
  (place-image (circle 20 'solid 'red) (world-x n) (world-y n) (empty-scene 800 800)))
;;(circle 20 'solid 'red)
;;
;(image=? (place-ball 200)
;         (place-image (circle 10 'solid 'red)
;                      100 200
;                      (empty-scene 200 200)))
;;
;(image=? (place-ball 0)
;         (place-image (circle 10 'solid 'red)
;                       100 0
;                       (empty-scene 200 200)))
;


(define bounce
  (lambda (wor)
    (cond
      ((or (and (< (world-vel-x wor) 0) (<= (world-x wor) 0))
           (and (> (world-vel-x wor) 0) (>= (world-x wor) (world-end wor))))
       (make-world
        (world-x wor)
        (world-y wor)
        (world-vel-y wor)
        (- (world-vel-x wor))
        (world-start wor)
        (world-end wor)
        (world-treshold wor)))
      (else wor))))

;; draw world -> scene
;; take whatever world represents and turn it into a scene
;; template
;; (define (draw w)
;; (... w ...))
(define (draw w)
  (place-ball w))
;; tick world -> world
;; advance the world by one time unit
;; increase world by 1, unless world >= 200
;; examples
;; (tick -1) -> 0
;; (tick 2) -> 3
;; (tick 99) -> 100
;; (tick 199) -> 200
;; (tick 200) -> 200
;; (tick 201) -> 201
;; template
;; (define (tick w)
;;   (cond
;;     ((< w 200) ...)
;;     ((> w 200) ...)
;;     (else ...)))



(define (tick w)
  (cond
    ((and (> (world-vel-y w) 0) (>= (world-y w) (world-end w)))
     (bounce (make-world (world-x w)
                 (world-y w)
                 (- (* 2 (/ (world-vel-y w) 3)))
                 (world-vel-x w) ;; vel-x
                 (/ (+ (world-start w) (world-end w)) 2.0)
                 (world-end w) 
                 (world-treshold w))))
    ((and (< (world-vel-y w) 0) (<= (world-y w) (world-start w)))
     (bounce (make-world (world-x w)
                 (world-y w)
                 (- (world-vel-y w)) 
                 (world-vel-x w) ;; vel-x
                 (world-start w)
                 (world-end w) (world-treshold w))))
    (else 
     (cond
       ((<= (abs (- (world-end w) (world-start w))) (world-treshold w)) w)
       (else
        (bounce (make-world 
         (+ (world-x w) (world-vel-x w))
         ;; changing the y coordinate by adding rearranged velocity... (* a/k velocity)
         (+ (world-y w) (* (world-vel-y w) (/ (+ 1 (abs (- (world-y w) (world-start w)))) (abs (- (world-end w) (world-start w))))))
         (world-vel-y w)
         (world-vel-x w) ;; vel-x
         (world-start w) 
         (world-end w) (world-treshold w))))))))


(define normalize
  (lambda (x)
    (cond
      ((= x 0) 1)
      (else
       (/ x (abs x))))))

(define key-handler
  (lambda (wor key)
    (cond
      ((key=? key "up")
       (bounce (make-world 
        (world-x wor)
        (world-y wor)
        (* 50 (normalize (world-vel-y wor))) ;; vel-y
        (world-vel-x wor) ;; vel-x
        0 ;; start
        (world-end wor)
        (world-treshold wor))))
      ((key=? key "left")
       (bounce (make-world
        (world-x wor)
        (world-y wor)
        (world-vel-y wor)
        (- (world-vel-x wor) 1)
        (world-start wor)
        (world-end wor)
        (world-treshold wor))))
      ((key=? key "right")
       (bounce (make-world
        (world-x wor)
        (world-y wor)
        (world-vel-y wor)
        (+ (world-vel-x wor) 1)
        (world-start wor)
        (world-end wor)
        (world-treshold wor))))
      (else (bounce wor)))))

(define mouse-handler
  (lambda (wor x y me)
    (cond
      ((and (symbol? me)
            (symbol=? me 'button-down))
       (make-world
        x y
        0 0 ;; y/x vels
        0 ;; start
        (world-end wor) ;; end is not changed anywhere
        (world-treshold wor) ;; is this really necessary??????????????????????????????*
        ))
      ((and (symbol? me)
            (symbol=? me 'button-up))
       (make-world
        x y
        50 0
        (world-start wor)
        (world-end wor)
        (world-treshold wor)))
      (else wor))))



;; big-bang number number number world
;; x : number : width of scene
;; y : number : y of scene
;; slice : number : length of time slice in seconds
;; w : world
(big-bang  (make-world 400 780 0 0 0 777 1)
           (to-draw draw)
           (on-mouse mouse-handler)
           (on-key key-handler)
           (on-tick tick 0.01))
