#lang racket
(provide slow-prime? prime?)

; slow prime 
(define (slow-prime? n)
    ; prime if the smallest divisor is equal to n
    (= n (slow-smallest-divisor n)))

(define (slow-smallest-divisor n)
    (define (find-divisor n test)
        (cond 
            ; square of test greater than n
            ((> (* test test) n) n)
            
            ; remainder of 4 / 2 = 0, 2 is smallest prime of 4
            ((= (remainder n test) 0) test)

            (else (find-divisor n (+ test 1)))
        )
    )
    ; kick off the "finding" with 2
    (find-divisor n 2))

; prime?
(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x) (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))