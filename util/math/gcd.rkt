; Greatest Common Divisor
; gcd(num1, num2) = gcd(num2, remainder of num1 / num2)
; GCD(206,40) = GCD(40,6)
;             = GCD(6,4)
;             = GCD(4,2)
;             = GCD(2,0)
;             =2

#lang racket
(provide gcd)

(define (gcd a b)
    (cond ((< a b) (gcd b a)) ; flip around if num1 smaller than num2
        ((= b 0) a)
        (else (gcd b (remainder a b)))))
