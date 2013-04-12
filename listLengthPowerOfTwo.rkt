#|

Specifically solves the problem of finding out if the 
length of the given list is a power of two ...
 
... WITHOUT USING any usual arithmetic operation ...

... BY implementing the necessary arithmetic on the length of the lists

Caner Derici
around 2008 (I think)
|#

#lang racket

(require htdp/testing)

;; l-num is a (listof any) where the length represents the value

(define sıfır? null?)

;; küçük-mü? : l-num l-num -> boolean
;; determines if the first l-num is smaller than the second one
(define (küçük-mü? l-num1 l-num2)
  (cond
    ((sıfır? l-num2) false)
    ((sıfır? l-num1) true)
    (else
     (küçük-mü? (cdr l-num1) (cdr l-num2)))))
;; tests
(check-expect (küçük-mü? (list 1) (list 1 2)) true)
(check-expect (küçük-mü? (list 2) empty) false)
(check-expect (küçük-mü? empty (list 1)) true)
;; important one
(check-expect (küçük-mü? empty empty) false)

;; çıkart : l-num l-num -> l-num
;; subtracts the second l-num from the first one
;; don't worry about the case that the second l-num is bigger than the first one,
;; because where çıkart is being used, that's already checked
;; -- we have no negative l-nums, so if we try (- 0 2), that would be 0
(define (çıkart l-num1 l-num2)
  (cond
    ((sıfır? l-num1) null)
    ((sıfır? l-num2) l-num1)
    (else
     (çıkart (cdr l-num1) (cdr l-num2)))))

;; tests
(check-expect (çıkart empty empty) empty)
(check-expect (çıkart (list 1 2) (list 1)) (list 2))
(check-expect (çıkart (list 1 2 3) (list 1 2)) (list 3))
(check-expect (çıkart (list 1 2) empty) (list 1 2))

;; divide-list-by-two : l-num l-num l-num -> (listof l-num l-num)
;; bölen is always (list 1 2)
;; (divide-list (list 1 2 3) null null) -> (list (list 1) (list 1))
;; (divide-list (list 3) (list 1) null) -> (list (list 1) (list 1))
;; (divide-list (list 1) null null) -> (list null (list 1))
;; (divide-list (list 1 2 3 4 5) null null) -> (list (list 1 1) (list 1))
(define (divide-list-by-two bölünen bölüm kalan)
  (cond
    ((küçük-mü? bölünen (list 1 2)) (list bölüm (cons 1 kalan)))
    ((null? (cdr (cdr bölünen))) (list (cons 1 bölüm) kalan))
    (else
     (divide-list-by-two (çıkart bölünen (list 1 2)) (cons 1 bölüm) kalan))))
;; test
(check-expect (divide-list-by-two (list 1 2 3) null null) (list (list 1) (list 1)))
(check-expect (divide-list-by-two (list 3) (list 1) null) (list (list 1) (list 1)))
(check-expect (divide-list-by-two (list 1 2 3 4 5) null null) (list (list 1 1) (list 1)))

(check-expect (divide-list-by-two (list 1 2 3 4 5 6) null null) (list (list 1 1 1) null))

(check-expect (divide-list-by-two (list 1 2 3 4) null null) (list (list 1 1) null))
(check-expect (divide-list-by-two (list 1 2) null null) (list (list 1) null))
(check-expect (divide-list-by-two (list 1) null null) (list null (list 1)))

;; found-or-go-on : (listof l-num l-num) -> boolean
(define (found-or-go-on bölüm&kalan)
  (cond
    ((null? (car bölüm&kalan))
     (null? (cdr (cadr bölüm&kalan))))
    (else
     (found-or-go-on (divide-list-by-two (car bölüm&kalan) null (cadr bölüm&kalan))))))

;; isPowerTwo : l-num -> boolean
(define (isPowerTwo l-num)
  (cond
    ((sıfır? l-num) false)
    (else
     (found-or-go-on (divide-list-by-two l-num null null)))))
;; tests
(check-expect (isPowerTwo (list)) false)
(check-expect (isPowerTwo (list 1)) true)
(check-expect (isPowerTwo (list 1 2)) true)
(check-expect (isPowerTwo (list 1 2 3)) false)
(check-expect (isPowerTwo (list 1 2 3 4)) true)
(check-expect (isPowerTwo (list 1 2 3 4 5 6)) false)
(check-expect (isPowerTwo (list 1 2 3 4 5 6 7 8)) true)
(check-expect (isPowerTwo (list 1 2 3 4 5 6 7 8 9 10)) false)

#| Performance Tests

(define test-list-16 (build-list 65536 (lambda(x)x)))
(define test-list-17 (append test-list-16 test-list-16))
(define test-list-18 (append test-list-17 test-list-17))
(define test-list-19 (append test-list-18 test-list-18))
(define test-list-20 (append test-list-19 test-list-19))
(check-expect (time (isPowerTwo test-list-16)) true)
(check-expect (time (isPowerTwo test-list-17)) true)
(check-expect (time (isPowerTwo test-list-18)) true)
(check-expect (time (isPowerTwo test-list-19)) true)
(check-expect (time (isPowerTwo test-list-20)) true)
|#
(generate-report)