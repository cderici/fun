#lang racket

;; The Famous Strategy Game NIM
;; 
;; Caner Derici
;;
;;
;; 
;; version 1.3 October 25, 2010
;;  - added exit functionality for executable
;;  - added UNDO
;;  - fancy legend
;;  - intelligent error messages ( - :
;;
;; version 1.0 October 24, 2010 

;; TODO
;; 
;; minor - unit-testing
;;
;; minor - interactions with user should be on the scene, not on stdout
;; 
;; MAJOR - there are spme small strategic bugs at finishing moves
;; example recreation tables -> (list 1 1 2 0), (list 1 2), (list 0 2 0 0)
;;
;; minor - functions should be reviewed and optimized 
;; (at least be turned into tail recursive ones)
;;
;; MAJOR - there might be some error recognizing mouse position on the 
;; third row
;;
;; minor (MAJOR at long term) - attack strategy (moving when nim-sum is 0) 
;; is too simple, there has to be a good one.
;; (current: take some random number of sticks from a randomly chosen row)

(require 2htdp/universe)
(require 2htdp/image)


;                                                                 
;                                                                 
;                                                                 
;     ;;;   ;;;;  ;;   ;  ;;;; ;;;;;;;   ;;   ;;   ;;;;;;;;  ;;;; 
;    ;   ;  ;  ;  ;;   ; ;;   ;   ;      ;;   ;;   ;   ;    ;;   ;
;   ;      ;    ; ;;;  ; ;        ;      ;;   ;;;  ;   ;    ;     
;   ;      ;    ; ; ;  ; ;;       ;     ;;;;  ; ;  ;   ;    ;;    
;   ;      ;    ; ; ;; ;  ;;;;    ;     ;  ;  ; ;; ;   ;     ;;;; 
;   ;      ;    ; ;  ; ;     ;;   ;     ;  ;  ;  ; ;   ;        ;;
;   ;      ;    ; ;  ;;;      ;   ;     ;;;;  ;  ;;;   ;         ;
;    ;   ;  ;  ;  ;   ;; ;   ;;   ;    ;    ; ;   ;;   ;    ;   ;;
;     ;;;   ;;;;  ;   ;;  ;;;;    ;    ;    ; ;   ;;   ;     ;;;; 
;                                                                 
;                                                                 
;                                                                 


;; match-stick image
(define match-image
  (underlay/xy (rectangle 5 50 'solid (make-color 250 220 100)) 
               -2 
               -2 
               (ellipse 10 15 'solid 'brown)))

;; (or 'player 'computer' 'comp-victory 'player-victory)
(define STARTER 'player)


;                                                                 
;                                                                 
;                                                                 
;   ;;;;     ;;  ;;;;;;;   ;;          ;;;;   ;;;;;; ;;;;;;  ;;;; 
;   ;   ;    ;;     ;      ;;          ;   ;  ;      ;      ;;   ;
;   ;    ;   ;;     ;      ;;          ;    ; ;      ;      ;     
;   ;    ;  ;;;;    ;     ;;;;         ;    ; ;      ;      ;;    
;   ;    ;  ;  ;    ;     ;  ;         ;    ; ;;;;;; ;;;;;;  ;;;; 
;   ;    ;  ;  ;    ;     ;  ;         ;    ; ;      ;          ;;
;   ;    ;  ;;;;    ;     ;;;;         ;    ; ;      ;           ;
;   ;   ;  ;    ;   ;    ;    ;        ;   ;  ;      ;      ;   ;;
;   ;;;;   ;    ;   ;    ;    ;        ;;;;   ;;;;;; ;       ;;;; 
;                                                                 
;                                                                 
;                                                                 

;; world
;; state -> symbol indicating whose turn is the current one (e.g. 'player/'computer)
;; table is a (listof rows)
;; memort : see below
(define-struct world (state table memory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INITIAL TABLE SETTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TABLE is a (listof row)
;; row : number (indicating the number of match-sticks are in that row)
(define main-table (list 1 3 5 7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; memory
;; this is just a short term memory, allowing the program to remember what has been 
;; done in the current turn in which the memory is changing
;; There are two reasons:
;; 1 - To check/prevent if the player wants to take sticks from multiple rows
;; 2 - UNDO functionality
;; row : a <number> indicating a row number (< row (length table))               --<---|
;; sticks : a <number> indicating the number of sticks taken from a particular row -->---|
;; (memory 0 0) indicates that no move's been made yet
(define-struct MEMORY (row sticks))


(define SCENE-WIDTH (* 50 (apply max main-table)))
(define SCENE-HEIGHT (* 90 (length main-table)))


;                                                                 
;                                                                 
;                                                                 
;     ;;   ;;;;;                      ;;;;;;; ;;;;;    ;;;  ;    ;
;     ;;     ;                           ;      ;     ;   ; ;   ; 
;     ;;     ;             ;             ;      ;    ;      ;  ;  
;    ;;;;    ;             ;             ;      ;    ;      ; ;   
;    ;  ;    ;             ;             ;      ;    ;      ;;;   
;    ;  ;    ;          ;;;;;;;          ;      ;    ;      ;  ;  
;    ;;;;    ;             ;             ;      ;    ;      ;  ;; 
;   ;    ;   ;             ;             ;      ;     ;   ; ;   ; 
;   ;    ; ;;;;;           ;             ;    ;;;;;    ;;;  ;    ;
;                                                                 
;                                                                 
;                                                                 


;; list-subtract : (listof number?) (listof number?) -> (listof number?)
;; example:
;; (list-subtract '(1 3 5) '(6 4 2)) : (list -5 -1 3)
(define (list-subtract l1 l2)
  (if (not (= (length l1) (length l2)))
      (error 'list-subtract "something went terribly wrong! look at bitwise-xor in calculate-new-table function")
      (list-subtract-helper l1 l2)))

(define (list-subtract-helper l1 l2)
  (cond
    ((null? l1) null)
    (else
     (cons (- (car l1) (car l2))
           (list-subtract-helper (cdr l1) (cdr l2))))))

;; locate-positive : (listof number?) number(initial-position<assumed to be 0 at start>) -> number
;; produces the location of the positive number in the list
;; ATTENTION!! This function assumes that only one positive number 
;; may be in the list, it might not be the case!
;; if all numbers are negative (i.e. nim-sum = 0), return -1
(define (locate-positive ls current-pos)
  (cond
    ((null? ls) -1) ;; attack strategy starts here
    ((positive? (car ls)) current-pos)
    (else
     (locate-positive (cdr ls) (add1 current-pos)))))

;; change-element : (listof number?)<table> number number -> (listof number?)<table>
;; index starts at 0
;; example
;; (change-element (list 1 3 5) 2 2) : (list 1 3 2)
(define (change-element ls pos new-val)
  (cond
    ((zero? pos) (cons new-val (cdr ls)))
    (else
     (cons (car ls) (change-element (cdr ls) (sub1 pos) new-val)))))

;; extract-non-zero-positions : (listof number?) number -> (listof number?)
;; example
;; (extract-non-zero-positions (list 1 0 1 0)) -> (0 2)
(define (extract-non-zero-positions table current-pos)
  (cond
    ((null? table) null)
    ((not (zero? (car table))) (cons current-pos
                                     (extract-non-zero-positions (cdr table) (add1 current-pos))))
    (else
     (extract-non-zero-positions (cdr table) (add1 current-pos)))))

;; calculate-new-table : table -> table
(define (calculate-new-table table)
  (let* ( ;; if table is (list 1 3 5)
         (nim-count (apply bitwise-xor table)) ;; -> 7
         (nim-sum (if (= nim-count 3) 2 nim-count)) ;;;;;;;; A DIRTY TRICK, I KNOW...
         (xor-each (map (lambda (x) (bitwise-xor nim-sum x)) table)) ;; (list 6 4 2)
         (difference-list (list-subtract table xor-each)) ;; (list -5 -1 3)
         (key-number-position-candidate (locate-positive difference-list 0)) ;; 2
         (key-number-position (if (= -1 key-number-position-candidate)
                                  (let ((non-zero-indices (extract-non-zero-positions table 0)))
                                    (list-ref non-zero-indices (random (length non-zero-indices))))
                                  key-number-position-candidate))
         (new-value (- (list-ref table key-number-position)
                       (if (= key-number-position-candidate -1)
                           (add1 (random (list-ref table key-number-position)))
                           (list-ref difference-list key-number-position)))) ;; 2
         )
    (change-element table key-number-position new-value)))


;; move : world -> world
(define (move w)
  (let ((new-table (calculate-new-table (world-table w))))
    (make-world 'player new-table (make-MEMORY 0 0))))

(define (tick w) 
  (cond
    ;; if the game is finished or it's player's turn -> do nothing
    ((or (symbol=? (world-state w) 'player-victory)
         (symbol=? (world-state w) 'comp-victory))
     ;;(symbol=? (world-state w) 'player))
     w)
    ;; if the table is empty -> set the state to 'comp-victory or 'player-victory
    ;; according to the last state
    ((foldr (lambda (x y) (and (zero? x) y)) true
            (world-table w))
     (make-world (if (symbol=? 'computer (world-state w))
                     'comp-victory
                     (if (symbol=? 'player (world-state w))
                         'player-victory
                         (error 'tick "state problemo, someone has to win! look at the state changings on event handlers..")))
                 (world-table w) 
                 (world-memory w)))
    ;; this check has to be here (below the finish test)
    
    ;; if it's computer's turn -> get moving
    ((symbol=? (world-state w) 'computer)
     (let ((new-world (begin 
                        (display "computer moves") (newline)
                        (move w))))
       (begin
         (display "new turn of player") (newline)
         new-world
         )))
    (else
     w)))


;                                                          
;                                                          
;                                                          
;   ;;;;   ;;;;;    ;;  ;     ; ;;;;;  ;;   ;   ;;;   ;;;; 
;   ;   ;  ;   ;;   ;;  ;  ;  ;   ;    ;;   ;  ;   ; ;;   ;
;   ;    ; ;    ;   ;;  ;  ;  ;   ;    ;;;  ; ;      ;     
;   ;    ; ;   ;;  ;;;; ; ;;; ;   ;    ; ;  ; ;      ;;    
;   ;    ; ;;;;;   ;  ; ; ; ; ;   ;    ; ;; ; ;   ;;  ;;;; 
;   ;    ; ;   ;   ;  ;  ;; ;;;   ;    ;  ; ; ;    ;     ;;
;   ;    ; ;    ;  ;;;;  ;; ;;    ;    ;  ;;; ;    ;      ;
;   ;   ;  ;    ; ;    ; ;   ;    ;    ;   ;;  ;   ; ;   ;;
;   ;;;;   ;     ;;    ; ;   ;  ;;;;;  ;   ;;   ;;;   ;;;; 
;                                                          
;                                                          
;                                                   


;; row->image : row(number) -> image
;; row is a list-of-booleans
(define (row->image a-row) 
  (cond
    ((zero? a-row) (rectangle 1 1 'outline 'white))
    (else
     (overlay/xy match-image 30 0
                 (row->image (sub1 a-row))))))


;; draw-table : table(listof number?) scene number -> scene
(define (draw-table a-table a-scene current-row)
  (cond
    ((null? a-table) a-scene)
    (else
     (place-image (row->image (first a-table))
                  (/ SCENE-WIDTH 2)
                  (* current-row 70)
                  (draw-table (cdr a-table) a-scene (add1 current-row))))))

;; draw : world -> scene
(define (draw w)
  (scale 1.5 
         (let* ((state (world-state w))
                (win-msg (if (symbol=? state 'comp-victory)
                             "I WON!"
                             (if (symbol=? state 'player-victory)
                                 "YOU WON!"
                                 "crap!"))))
           (cond
             ((string=? win-msg "crap!") ;; game continues
              ;; render control info
              (place-image (above/align "left"
                                        (beside (text "press " 7 'darkgreen)
                                                (text "r" 8 'red)
                                                (text " to reset game at any time" 7 'darkgreen))
                                        (beside (text "press " 7 'darkgreen)
                                                (text "space" 7 'red)
                                                (text " to pass the turn" 7 'darkgreen))
                                        (beside (text "press " 7 'darkgreen)
                                                (text "backspace" 7 'red)
                                                (text " to undo a move" 7 'darkgreen))
                                        (beside (text "press " 7 'darkgreen)
                                                (text "esc" 7 'red)
                                                (text " to exit" 7 'darkgreen))
                                        (text "use the mouse!" 7 'darkgreen)) 
                           85 
                           40
                           ;; render who has the current turn
                           (place-image 
                            (text (string-append 
                                   (symbol->string (world-state w))
                                   "'s turn") 10 'blue)
                            (- SCENE-WIDTH 50)
                            10
                            ;; finally render the table
                            (draw-table (world-table w) (empty-scene SCENE-WIDTH SCENE-HEIGHT) 1))))
             (else ;; game's finished
              (place-image (above 
                            (text win-msg (round (/ (+ SCENE-HEIGHT SCENE-WIDTH) 15)) 'red)
                            (text "Press r to reset the game."
                                  15
                                  'blue))
                           (/ SCENE-WIDTH 2)
                           (/ SCENE-HEIGHT 2)
                           (empty-scene SCENE-WIDTH SCENE-HEIGHT)))))))




;                                            
;                                            
;                                            
;   ;;;;;; ;    ; ;;;;;; ;;   ;;;;;;;;  ;;;; 
;   ;      ;    ; ;      ;;   ;   ;    ;;   ;
;   ;       ;  ;  ;      ;;;  ;   ;    ;     
;   ;       ;  ;  ;      ; ;  ;   ;    ;;    
;   ;;;;;;  ;  ;  ;;;;;; ; ;; ;   ;     ;;;; 
;   ;       ;  ;  ;      ;  ; ;   ;        ;;
;   ;        ;;   ;      ;  ;;;   ;         ;
;   ;        ;;   ;      ;   ;;   ;    ;   ;;
;   ;;;;;;   ;;   ;;;;;; ;   ;;   ;     ;;;; 
;                                            
;                                            
;                                            

;; undo : world -> world
;; returns ro the first state of the memory by incresing the particular 
;; row(MEMORY-row) in the table by the number indicated by MEMORY-sticks
(define (undo w)
  (let (
        (table (world-table w))
        (zero-based-index-of-row-in-use (sub1 (MEMORY-row (world-memory w)))) ;; change-element wants an index based on 0
        ) 
    (if (= -1 zero-based-index-of-row-in-use)
        w
        (make-world (world-state w)
                    (change-element table 
                                    zero-based-index-of-row-in-use
                                    (+ (list-ref table zero-based-index-of-row-in-use)
                                       (MEMORY-sticks (world-memory w))))
                    (make-MEMORY 0 0)))))


;; key : world key -> world
;; ATTENTION : 'space SWICTHES THE WORLD STATE from player to computer
;; the key 'r' resets the play
(define (key w k)
  (cond
    ((key=? k "escape") 
     (begin 
       (display "exiting..")
       (exit 0)))
    ((key=? k "\b") 
     (begin
       (display "trying to undo the move\n")
       (undo w)))
    ((key=? k "r")
     (begin
       (display "resetting game") (newline)
       (make-world STARTER main-table (make-MEMORY 0 0))))
    ((and (key=? k " ") (symbol=? (world-state w) 'player))
     (if (and (zero? (MEMORY-row (world-memory w)))
              (zero? (MEMORY-sticks (world-memory w))))
         (begin
           (display "you didn't move yet!\n")
           w)
         (begin
           (display "new turn of computer") (newline)
           (make-world 'computer (world-table w) (make-MEMORY 0 0)))))
    (else w)))

;; which-row-is-been-clicked : world number -> number
(define (which-row-is-been-clicked w y)
  (let* ((height SCENE-HEIGHT)
         (rows (length (world-table w)))
         (scale (/ height rows))
         (row-num (+ 1 (quotient y scale))))
    (begin
      (display "clicked row is : ")
      (display row-num)
      (newline)
      row-num)))

;; decrease-a-row : number table -> table
(define (decrease-a-row row-num table)
  (cond
    ((= 1 row-num) (cons (if (zero? (car table))
                             0
                             (sub1 (car table))) (cdr table)))
    (else
     (cons (car table) (decrease-a-row (sub1 row-num) (cdr table))))))

;; button-clicked : world number number -> world
(define (button-clicked w x y)
  (let ((row-num (which-row-is-been-clicked w y)))
    (cond
      ;; checking if the player wants to take a stick from an already empty row
      ((= (list-ref (world-table w) (sub1 row-num)) 0)
       (begin
         (display (string-append "that row is already empty!\n"
                                 (if (= row-num (MEMORY-row (world-memory w)))
                                     "you might want to pass the turn.. or undo your move..\n"
                                     "")))
         w))
      ;; checking if player tries to take from multiple rows at the same turn
      ((or (= row-num (MEMORY-row (world-memory w))) (= (MEMORY-row (world-memory w)) 0))
       ;; remembering the row
       ;; ANOTHER DIRTY TRICK HERE
       (let* ((new-table (decrease-a-row row-num (world-table w)))
              (non-zero-rows (extract-non-zero-positions new-table 0))
              )
         (make-world (if (zero? (length non-zero-rows))
                         'comp-victory
                         (world-state w))
                     new-table
                     (make-MEMORY row-num (add1 (MEMORY-sticks (world-memory w)))))))
      (else
       (begin
         (display "\nyou are allowed to take from only one row\n")
         w)))))


;; mouse : world x y me
(define (mouse w x y me)
  (cond
    ((and (mouse=? me "button-down") (symbol=? (world-state w) 'player))
     (button-clicked w x y))
    (else w)))


;; INITIALIZE

(big-bang (make-world STARTER main-table (make-MEMORY 0 0))
          (on-tick tick)
          (on-draw draw)
          (on-key key)
          (on-mouse mouse))
