#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(define width 600)
(define height 200)

(define number-of-letters 20)

;; world -> (listof letter?)

;; ascii : number
;; ok? : boolean
(struct letter (ascii ok?))


(define (gen-letter)
  (+ 65 (random 25)))

(define init-world (build-list number-of-letters
                               (lambda (x)
                                 (letter (gen-letter) false))))

(define (modify-letter lttr)
  (if (letter-ok? lttr)
      lttr
      (letter (gen-letter) false)))

(define (tick w)
  (map modify-letter w))

;; place-letters : world number number scene -> scene
(define (place-letters w interval which-interval? scn)
  (cond
    ((empty? w) scn)
    (else
     (place-letters
      (cdr w)
      interval
      (add1 which-interval?)
      (place-image (text (string (integer->char (letter-ascii (first w))))
                         (round (/ interval 1)) 'black)
                   (+ (/ interval 2)
                      (* which-interval? interval))
                   (/ height 2) scn)))))

(define (fix-letter w)
  (cond
    ((empty? w) empty)
    ((letter-ok? (car w)) (cons (car w)
                                (fix-letter (cdr w))))
    (else
     (cons (letter (letter-ascii (car w))
                   true) (cdr w)))))
    

(define (mouse w x y me)
  (cond
    ((and (mouse=? me "button-down")
          (empty? (filter (compose not letter-ok?)
                          w)))
     init-world)
    ((mouse=? me "button-down")
       (fix-letter w))
    (else w)))
          
                
(define (draw w)
  (place-image
   (text "Click!" 30 'blue)
   (* width 0.5)
   (- height 50)
   (place-letters w (/ width (length w)) 0 (empty-scene width height))))


(big-bang init-world 
          (on-tick tick)
          (to-draw draw)
          (on-mouse mouse))

