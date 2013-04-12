#lang plai

#|
This is a very simple interpreter for the fully bracketed lambda calculus:
L -> i
L -> (L L)
L -> (λ i L)

What it does in the end is to compute (factorial 4) in lambda calculus with 
Church Numerals using Y combinator (it is lazy, yes), one thing that is 
tremendously hard to do in C or Java :)

Caner Derici
Summer 2011
Nesin Math Village, Selçuk, Izmir, Turkey
|#

(define-type L
  [id (s symbol?)]
  [app (fun-expr L?) (arg-expr L?)]
  [def (param symbol?) (body L?)])

; parse: s-expr -> L-expr
(define (parse sexp)
  (cond
    ((symbol? sexp) (id sexp))
    ((and (list? sexp) (= (length sexp) 3) (symbol=? (first sexp) 'λ))
     (def (second sexp) (parse (third sexp))))
    ((and (list? sexp) (= (length sexp) 2))
     (app (parse (first sexp))
          (parse (second sexp))))
    (else (error 'parse "invalid expression"))))

;; unparse: λ-expr -> s-expr
;; for debugging purposes
(define (unparse lexpr)
  (type-case L lexpr
    [id (v) v]
    [app (p q) (list (unparse p) (unparse q))]
    [def (param body) `(λ ,param ,(unparse body))]))

;; set-union : (listof symbol?) (listof symbol?) -> (listof symbol?)
(define (set-union l1 l2)
  (make-unique (append l1 l2)))

;; make-unique : (listof symbol?) -> (listof symbol?)
(define (make-unique los)
  (cond
    ((null? los) null)
    (else
     (cons (car los)
           (make-unique (filter (lambda (x) (not (symbol=? x (car los)))) (cdr los)))))))
(test (make-unique '(a b c a b b k)) '(a b c k))

;; FI : λ-expr -> (listof symbol?)
(define (FI l-expr)
  (type-case L l-expr
    [id (s) (list s)]
    [app (p q) (set-union (FI p) (FI q))]
    [def (i b) (filter (lambda (x) (not (symbol=? x i))) (FI b))]))

;; BI : λ-expr -> (listof symbol?)
(define (BI l-expr)
  (type-case L l-expr
    [id (s) (list)]
    [app (p q) (set-union (BI p) (BI q))]
    [def (i b) (set-union (BI b) (list i))]))

;; subst : L-expr symbol L-expr -> L-expr
(define (subst M i N)
  (type-case L M
    [id (j) (if (symbol=? i j) N M)]
    [app (P Q) (app (subst P i N) (subst Q i N))]
    [def (j P)
      (if (symbol=? i j) 
          M
          (if (and (member i (FI P)) (member j (FI N)))
              (local ((define new-sym (gensym)))
                (def new-sym (subst (subst P j (id new-sym)) i N)))
              (def j (subst P i N))))]))

(test (subst (id 'i) 'i (id 'k)) (id 'k))
(test (subst (id 'i) 'j (id 'k)) (id 'i))

;; B-subst-one-step : λ-expr -> λ-expr
(define (B-subst-one-step lexpr)
  (type-case L lexpr
    [id (v) lexpr]
    [app (p q)
         (cond
           ((def? p) (subst (def-body p) (def-param p) q))
           ((id? p) (app p (B-subst-one-step q)))
           (else (app (B-subst-one-step p) q)))]
    [def (param b)
      (def param (B-subst-one-step b))]))

(test (B-subst-one-step (parse '((λ x (x x)) (λ x (x x))))) (parse '((λ x (x x)) (λ x (x x)))))
(test (B-subst-one-step (parse '(((λ j j) o) k))) (parse '(o k)))
(test (B-subst-one-step (parse '(λ j ((λ o o) j)))) (parse '(λ j j)))
(test (B-subst-one-step (parse '(o ((λ j j) k)))) (parse '(o k)))

(test (B-subst-one-step (parse '((λ t (((λ h h) o) t)) p)))
      (parse '(((λ h h) o) p)))
(test (B-subst-one-step (parse '((λ t (((λ h h) o) t)) ((λ i i) u))))
      (parse '(((λ h h) o) ((λ i i) u))))


;; B-subst : λ-expr -> λ-expr
(define (B-subst lexpr)
  [type-case L lexpr
    [id (v) lexpr]
    [app (p q)
         (if (id? p)
             (app p (B-subst (B-subst-one-step q)))
             (B-subst (B-subst-one-step lexpr)))]
    [def (param body) 
      (def param (B-subst body))]])

(test (B-subst (parse '(a b))) (parse '(a b)))
(test (B-subst (parse '(((λ u u) o) (p g)))) (parse '(o (p g))))
(test (B-subst (parse '(p g))) (parse '(p g)))
(test (B-subst (parse '(p ((λ h h) g)))) (parse '(p g)))
(test (B-subst (parse '(((λ h h) p) g))) (parse '(p g)))

(define omega '((λ x (x x)) (λ x (x x))))

(test (B-subst (parse `((λ t (t ,omega)) ((λ i (λ y o)) ,omega))))
      (parse 'o))

(test (B-subst (parse '(((λ u (λ k i)) o) ((λ j (j j)) (λ x x))))) (parse 'i))
(test (B-subst (parse '((λ t (((λ h h) o) t)) p))) (parse '(o p)))


(define zero '(λ f (λ x x)))
(define one '(λ f (λ x (f x))))
(define two '(λ f (λ x (f (f x)))))
(define three '(λ f (λ x (f (f (f x))))))
(define four '(λ f (λ x (f (f (f (f x)))))))
(define five '(λ f (λ x (f (f (f (f (f x))))))))
(define six '(λ f (λ x (f (f (f (f (f (f x)))))))))

(define SUCC '(λ n (λ f (λ x (f ((n f) x))))))

(define ADD `(λ m (λ n ((m ,SUCC) n))))

(test (B-subst (app (parse SUCC) (parse one))) (parse two))

(test (B-subst (app (app (parse ADD) (parse one)) (parse two))) (parse three))

;; pred;

(define TRUE '(λ x (λ y x)))
(define FALSE '(λ x (λ y y)))

(define CONS '(λ x (λ y (λ f ((f x) y)))))
(define FIRST `(λ p (p ,TRUE)))
(define SECOND `(λ p (p ,FALSE)))

(define IF '(λ test (λ truth (λ falsity ((test truth) falsity)))))

(define ZERO? `(λ n ((n (λ x ,FALSE)) ,TRUE)))

(test (B-subst (parse `(,ZERO? ,zero))) (parse TRUE))

(define step `(λ x ((,CONS (,SECOND x)) (,SUCC (,SECOND x)))))

(define PRED `(λ n (,FIRST ((n ,step) ((,CONS ,zero) ,zero)))))

(test (B-subst (parse `(,PRED ,two))) (parse one))
(test (B-subst (parse `(,PRED ,three))) (parse two))
(test (B-subst (parse `(,PRED ,four))) (parse three))
(test (B-subst (parse `(,PRED ,six))) (parse five))

;; mul

(define MUL `(λ m (λ n (λ f (m (n f))))))

(test (B-subst (parse `((,MUL ,two) ,three))) (parse six))


(define Y '(λ f ((λ x (f (x x))) (λ x (f (x x))))))

(define fact `(,Y (λ fact (λ n (((,IF (,ZERO? n)) ,one) ((,MUL n) (fact (,PRED n))))))))

(B-subst (parse `(,fact ,four)))