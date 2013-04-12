#lang racket

;; Caner Derici, December 2011

(require 2htdp/universe)
(require 2htdp/image)
(require "grid-gen.rkt")

;; always square
(define dim 20) ;; there are 20 cells for each row and column
(define cell-dim 20) ;; and each cell is 20x20 pixels
(define scene-dim (let ((default-dim (* dim cell-dim)))
                    (if (= 0 (modulo dim 2))
                        default-dim
                        (+ default-dim cell-dim))))

(define init true)

(define-struct cell (x y))

(define-struct world (grid current-cell main-stack))

(define my-grid (grid-generator dim))

(define init-world (make-world my-grid (make-cell 0 0) null))
;(vector-set! (vector-ref world 6) 4 1)

#|
BEGIN : DRAWING THE GRID
|#
(define (draw wor)
  (let ((grd (world-grid wor))
        (current-cell (world-current-cell wor)))
    (draw-rows grd 0 (empty-scene scene-dim scene-dim) current-cell)))

(define (draw-rows grd index scn c-cell)
  (cond
    ((= index dim) scn)
    (else
     (draw-rows grd
                (add1 index)
                (place-row (vector-ref grd index)
                           0 ;start-index
                           index ;which row is this?
                           scn c-cell)
                c-cell))))

(define (place-row a-vector start-index row-index scene c-cell)
  (cond
    ((= start-index dim) scene)
    (else
     (place-row a-vector
                (add1 start-index)
                row-index
                (if (= 1 (vector-ref a-vector start-index))
                    (place-image (rectangle cell-dim cell-dim 'solid
                                            (if (and (= row-index (cell-y c-cell))
                                                     (= start-index (cell-x c-cell)))
                                                'red
                                                'black)
                                            ;(make-color (random 255) (random 255) (random 255)))
                                            )
                                 (+ cell-dim (* start-index cell-dim))
                                 (+ cell-dim (* row-index cell-dim))
                                 scene)
                    scene)
                c-cell))))
#|
END : DRAWING THE GRID
|#

#|
BEGIN : TRAVERSING THE GRID
|#

;; clean-visiteds : (listof (listof [0,9] [0,9])) (vectorof (vectorof {0,1,2})) -> (listof (listof [0,9] [0,9]))
(define (clean-visiteds possible-neighbors a-grid)
  (filter (lambda (a-neighbor)
            (= 0 (vector-ref
                  (vector-ref a-grid (second a-neighbor))
                  (first a-neighbor))))
          possible-neighbors))

;; clean-up : (listof (listof number number)) (vectorof (vectorof {0,1,2})) -> (listof (listof [0,9] [0,9]))
;; removes the cell positions out of boundaries 
;; and
;; removes the cell positions that are already 1(visited before) on grid
(define (clean-up neighbors a-grid)
  (clean-visiteds
   (filter (lambda (a-neighbor)
             (not (or (negative? (first a-neighbor))
                      (negative? (second a-neighbor))
                      (>= (first a-neighbor) dim)
                      (>= (second a-neighbor) dim))))
           neighbors)
   a-grid))

;; choose-neighbor : (vectorof (vectorof {0,1,2})) cell -> cell
(define (choose-neighbor a-grid current-cell)
  (let* ((c-x (cell-x current-cell))
         (c-y (cell-y current-cell))
         (neighbors (clean-up (list
                               (list (- c-x 2) c-y)
                               (list c-x (+ 2 c-y))
                               (list (+ 2 c-x) c-y)
                               (list c-x (- c-y 2))) a-grid)))
    (if (null? neighbors)
        (values false 0)
        (values (list-ref neighbors (random (length neighbors)))
                (- (length neighbors) 1)))))

;; set-new-neighbor-visited! : (vectorof (vectorof {0,1,2})) cell -> (vectorof (vectorof {0,1,2}))
(define (set-new-neighbor-visited! a-grid a-cell)
  (begin
    (vector-set! (vector-ref a-grid (cell-y a-cell)) (cell-x a-cell) 1)
    a-grid))

;; break-the-wall : (vectorof (vectorof {0,1,2})) cell cell -> (vectorof (vectorof {0,1,2}))
(define (break-the-wall a-grid new-cell current-cell)
  (let* ((new-x (cell-x new-cell))
         (new-y (cell-y new-cell))
         (cur-x (cell-x current-cell))
         (cur-y (cell-y current-cell))
         ; wall to be breaked down
         (wall-x (if (> new-x cur-x) (add1 cur-x) (if (= new-x cur-x) cur-x (sub1 cur-x))))
         (wall-y (if (> new-y cur-y) (add1 cur-y) (if (= new-y cur-y) cur-y (sub1 cur-y)))))
    (begin
      ;(vector-set! (vector-ref a-grid wall-x) wall-y 1)
      ; change the line below with the line above if you want to hide the creation of the maze
      (vector-set! (vector-ref a-grid wall-y) wall-x 1)
      a-grid)))

(define (tick-new w)
  ; determine the neighbor randomly
  ; new-cell : a cell
  ; noun : number of unvisited neighbors remaining
  (let-values ([(new-cell-pos nOun) (choose-neighbor (world-grid w) (world-current-cell w))])
    (cond
      ;; stack is empty- terminate
      ((and (not init) (null? (world-main-stack w))) w)
      ((not new-cell-pos) ;there's nowhere to go
       (make-world
        (world-grid w)
        (car (world-main-stack w))
        (cdr (world-main-stack w))))
      (else
       (let ((new-cell (make-cell (first new-cell-pos)
                                  (second new-cell-pos))))
         (begin
           (set! init false)
           (make-world
            ; make the choosen neighbor visited (set it to 1 in grid)
            ; and break the wall between the new-cell and the current-cell
            (set-new-neighbor-visited!
             (break-the-wall (world-grid w) new-cell (world-current-cell w))
             new-cell)
            new-cell
            (if (zero? nOun)
                (world-main-stack w)
                (begin
                  ;(display nOun)
                  ;(newline)
                  (cons new-cell (world-main-stack w)))))))))))

#|
END : TRAVERSING THE GRID
|#

;(world-grid init-world)

(define my-maze (big-bang init-world (on-tick tick-new 1/28)
                          (to-draw draw)))