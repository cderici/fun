#lang racket

#|
Producing prime numbers using eratosthene's sieve
|#

(define (general-producer body)
  (local ((define resume (box false)))
    (lambda (real-send)
      (local ((define send-to (box real-send))
              (define send (lambda (value-to-send)
                             (set-box! send-to
                                       (let/cc k
                                         (begin
                                           (set-box! resume k)
                                           ((unbox send-to) value-to-send)))))))
        (if (unbox resume)
            ((unbox resume) real-send)
            (body send)
            )))))


(define (div? n lop)
  (cond
    ((null? lop) true)
    ((and (not (= 1 (car lop)))
          (= (modulo n (car lop)) 0)) false)
    (else (div? n (cdr lop)))))

(define sieve-body
  (local ((define memory null))
    (lambda (send)
      (local ((define (loop n)
                (if (div? n memory)
                    (begin
                      (set! memory (cons n memory))
                      (send n)
                      (loop (add1 n)))
                    (loop (add1 n))))
              )
        (loop 1)))))

(define sieve
  (general-producer sieve-body))


(call/cc sieve)

(call/cc sieve)

(call/cc sieve)

(call/cc sieve)

(call/cc sieve)

(call/cc sieve)

(call/cc sieve)

(call/cc sieve)

(call/cc sieve)