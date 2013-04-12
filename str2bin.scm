;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname str2bin) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))

#|
Partly useful if you want to communicate in binary.

Caner Derici
late 2005
|#

;; int2str : number -> string
;; convert any integer to 8-bit binary format represented as a string
;; (int2str 97) -> "01100001"
;; (int2str 107) -> "01101011"
(define (int2str n)
  (local ((define mod (modulo n 2))
          (define quot (quotient n 2)))
    (cond
      ((= quot 1) (string-append "0"
                   (number->string mod)
                   (number->string quot)))
      (else
       (string-append
        (int2str quot)
        (number->string mod))))))
(check-expect (int2str 97) "01100001")
(check-expect (int2str 107) "01101011")

;; str2bin : string -> string
;; converts the written string to complete 8-bit binary form
;; (str2bin "caner") -> "0110001101100001011011100110010101110010"
(define (str2bin str)
  (foldr string-append "" (map int2str (map string->int (explode str)))))
(check-expect (str2bin "caner") "0110001101100001011011100110010101110010")


;; bin2str : string -> string
;; converts the given string as 8-bit binary stream to the corresponding string
(define (bin2str bin-str)
  (implode (map int->string (map bin2int (bin2list8bit bin-str)))))
(check-expect (bin2str "0110001101100001011011100110010101110010") "caner")

;; bin2list8bit : string -> list-of-string
;; (bin2list8bit "0110001101100001011011100110010101110010") -> (list "01100011" "01100001" "01101110" "01100101" "01110010")
(define (bin2list8bit str)
  (cond
    ((not (= (modulo (string-length str) 8) 0)) (error 'bin2list8bit "given str is not appropirate"))
    (else
     (reverse (helper str null)))))

(check-expect (bin2list8bit "0110001101100001011011100110010101110010") (list "01100011" "01100001" "01101110" "01100101" "01110010"))
(check-error (bin2list8bit "0110001101100001011011100110010") "bin2list8bit: given str is not appropirate")

(define (helper str outlist)
  (cond
    ((string=? str "") outlist)
    (else
     (helper (substring str 8 (string-length str))
             (cons (substring str 0 8) outlist)))))

;; bin2int : string -> number
;; converts the given 8-bit binary number as string to the corresponding decimal number
;; (bin2int "01100001") -> 97
(define (bin2int str)
  (inner (reverse (explode (substring str 1 8))) 0 0))
(check-expect (bin2int "01100001") 97)

;; inner : list-of-string number number -> number
;; processes individual elements of the given list-of-strings to 
;; accumulate the corresponding decimal on out.
(define (inner los counter out)
  (cond
    ((null? los) out)
    (else
     (inner (cdr los) (add1 counter) 
            (if (string=? (car los) "1")
                (+ out (expt 2 counter))
                out)))))

;; general test: "caner" "0110001101100001011011100110010101110010"
(check-expect (bin2str (str2bin "caner")) "caner")

(bin2str "011011010110100101101110011010010110111001100001")