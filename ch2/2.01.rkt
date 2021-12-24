#lang racket
(require (submod "../util/math.rkt"))

(define (make-rat n d)
    (let ((g (gcd (abs n) (abs d))))
        (cond ((< (* n d) 0) (cons (* -1 (abs (/ n g))) (/ (abs d) g)))
            (else (cons (abs (/ n g)) (abs (/ d g)))))))

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

(make-rat -1 2) ; '(-1 . 2)
(make-rat 1 -3) ; '(-1 . 3)
(make-rat -1 -4) ; '(1 . 4)
(make-rat -2 4) ; '(-1 . 2)




