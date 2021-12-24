#lang sicp
; • (make-rat ⟨n⟩ ⟨d⟩) returns the rational number whose numerator is the integer ⟨n⟩ and whose denominator is the integer ⟨d⟩.
; • (numer ⟨x⟩) returns the numerator of the rational number ⟨x⟩. 
; • (denom ⟨x⟩) returns the denominator of the rational number ⟨x⟩.

(define x (cons 1 2)) ; cons = construct
(car x) ; 1 ; car = address of register
(cdr x) ; 2 ; cdr = decrement of register

(define (gcd a b)
    (cond ((< a b) (gcd b a))
        ((= b 0) a)
        (else (gcd b (remainder a b)))))

(define (make-rat n d)
    (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))

(define (numer x)
    (car x))

(define (denom x)
    (cdr x))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))

(define (add-rat x y)
    (make-rat 
            (+ (* (denom y) (numer x))
                (* (denom x) (numer y)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat 
            (- (* (denom y) (numer x))
                (* (denom x) (numer y)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat 
            (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
    (make-rat 
            (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
    (= (* (numer x) (denom y))
        (* (numer y) (denom x))))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

(print-rat one-half)
; 1/2
(print-rat one-third)
; 1/3

; Add Rational Numbers
(print-rat (add-rat one-half one-third))
; 5/6
; Subtract Rational Numbers
(print-rat (sub-rat one-half one-third))
; 1/6
; Multiply Rational Numbers
(print-rat (mul-rat one-half one-third))
; 1/6
; Divide Rational Numbers
(print-rat (div-rat one-half one-third))
; 3/2

; Rounding Rational Numbers
(print-rat (add-rat one-third one-third))
; 6/9 -> 2/3