#lang sicp

(define a 1)
(define b 2)
(list a b) ; (1 2)

(list 'a b) ; (a 2)
(list a 'b) ; (1 b)
(list 'a 'b) ; (a b)

(car '(a b c)) ; a
(cadr '(a b c)) ; b

(eq? 'a 'a) ; #t
