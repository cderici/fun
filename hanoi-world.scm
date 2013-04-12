#|
Visual solution of the famous Tower of Hanoi problem

Caner Derici
before 2006
|#

#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(define speed 20)
(define num-of-disks 8)

(define-struct obj (image x y))

;; env -> list (the little lines at bottom)
;; rod1 2 3 -> list (the three rods of the tower)
(define-struct world (env rod1 rod2 rod3))

;; place-images : list-of-images -> scene
(define (place-images ls)
  (cond
    ((null? ls) (empty-scene 700 700))
    (else
     (place-image (obj-image (car ls)) 
                  (obj-x (car ls))
                  (obj-y (car ls))
                  (place-images (cdr ls))))))

;; place-disks : list-of-numbers number scene -> scene
(define (place-disks lon x-pos sc)
  (letrec ((inner (lambda (ls xpos scene l k)
                   (cond
                     ((null? ls) scene)
                     (else
                      (place-image (circle (* (car ls) 10) 'solid 'red)
                                   xpos
                                   (- (* (/ 600 l) (add1 (- l k))) 50)
                                   (inner (cdr ls) xpos scene l (sub1 k))))))))
  (inner lon x-pos sc (length lon) (length lon))))
                            
;; draw : world -> scene
(define (draw w)
  (place-disks (world-rod1 w) 150
               (place-disks (world-rod2 w) 350
                            (place-disks (world-rod3 w) 550 (place-images (world-env w))))))



;; switch-elements : list-of-number list-of-number list-of-number number number -> list-of-(list-of-number)
;; f -> from
;; t -> to
;;;; conditional template here:
;;(define (switch-elements r1 r2 r3 f t)
;;  (cond
;;    ((= f 1) ...)
;;    ((= f 2) ...)
;;    ((= f 3) ...)))
(define (switch-elements r1 r2 r3 f t)
  (cond
    ((= f 1)
     (if (= t 2)
         (list (cdr r1) (cons (car r1) r2) r3)
         (list (cdr r1) r2 (cons (car r1) r3))))
    ((= f 2)
     (if (= t 1)
         (list (cons (car r2) r1) (cdr r2) r3)
         (list r1 (cdr r2) (cons (car r2) r3))))
    ((= f 3)
     (if (= t 1)
         (list (cons (car r3) r1) r2 (cdr r3))
         (list r1 (cons (car r3) r2) (cdr r3))))))

;; tick : world -> world
;;(define (tick w)
;;  (cond
;;    ((null? solution-list) ... w ...)
;;    (else ... w ...)))
(define (tick w)
  (cond
    ((null? solution-move-list) w)
    (else
     (simulate-move w))))

(define (simulate-move w)
  (letrec ((next-move (car solution-move-list))
           (from (car next-move))
           (to (cadr next-move))
           ;; rods is a list!!!
           (rods (switch-elements (world-rod1 w)
                                  (world-rod2 w)
                                  (world-rod3 w) from to)))
    (begin
      (set! solution-move-list (cdr solution-move-list))
      (make-world
       (world-env w)
       (car rods)
       (cadr rods)
       (caddr rods)))))

(define (range n)
  (letrec ((inner (lambda (n k)
                   (cond
                     ((= n k) (cons k null))
                     (else
                      (cons k (inner n (add1 k))))))))
    (inner n 1)))

(define init-world
  (make-world (list
               (make-obj (line 100 0 'black) 100 650)
               (make-obj (line 0 650 'black) 150 0)
               (make-obj (line 100 0 'black) 300 650)
               (make-obj (line 0 650 'black) 350 0)
               (make-obj (line 100 0 'black) 500 650)
               (make-obj (line 0 650 'black) 550 0))
              (range num-of-disks)
              null
              null))

(define (hanoi n Ax Cx Bx)
  (cond
    ((= n 1) (list (list Ax Cx)))
    (else
     (append
       (hanoi (sub1 n) Ax Bx Cx)
       (hanoi 1 Ax Cx Bx)
       (hanoi (sub1 n) Bx Cx Ax)))))

(define solution-move-list
  (hanoi (length (world-rod1 init-world)) 1 3 2))

(big-bang init-world (on-tick tick (/ 1 speed)) (to-draw draw))