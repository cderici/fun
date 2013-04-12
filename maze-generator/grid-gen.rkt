#lang racket

(provide grid-generator)

;; grid codes
;; 0 : unvisited cell
;; 1 : visited cell
;; 2 : wall


(define (raise-wall-in-odds row i)
  (cond
    ((>= i (vector-length row)) row)
    ((= 0 (modulo i 2)) (raise-wall-in-odds row (add1 i)))
    (else
     (raise-wall-in-odds (begin 
                           (vector-set! row i 2)
                           row) (add1 i)))))

(define (put-vert-walls grid i)
  (cond
    ((>= i (vector-length grid)) grid)
    ((= 0 (modulo i 2)) (put-vert-walls
                         (begin (vector-set! grid i (raise-wall-in-odds (vector-ref grid i) 0)) grid)
                         (add1 i)))
    (else
     (put-vert-walls grid (add1 i)))))

(define (put-horz-walls grid-skeleton i)
  (cond
    ((>= i (vector-length grid-skeleton)) grid-skeleton)
    ((= 0 (modulo i 2)) (put-horz-walls grid-skeleton (add1 i)))
    (else
     (put-horz-walls (begin (vector-set! grid-skeleton i (make-vector (vector-length grid-skeleton) 2))
                            grid-skeleton) (add1 i)))))
  

;; dim is based on the cells, not the overall vector lengths
;; actual vector lengths are dim*2
(define (grid-generator dim)
  (let* ((skeleton (vector-map (lambda (x) (make-vector dim)) (make-vector dim)))
         (skeleton+horz-walls (put-horz-walls skeleton 0))
         (cells&walls (put-vert-walls skeleton+horz-walls 0))
         )
    ; initialize
    (begin (vector-set! (vector-ref cells&walls 0) 0 1) cells&walls)))