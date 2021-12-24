#lang sicp

(define (cons x y)
    (lambda (pick) 
        (cond ((= pick 0) x)
            ((= pick 1) y)
            (else (error "Argument not 0 or 1: CONS")))))

; z is a lambda returned by cons
(define (car z) (z 0))
(define (cdr z) (z 1))

(define z (cons 1 2))
(car z) ; 1
(cdr z) ; 2
