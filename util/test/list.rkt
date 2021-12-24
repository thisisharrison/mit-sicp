#lang racket
(require (submod "../list.rkt") (submod "../math.rkt"))

(define test (list 1 2 3 4))

(index test 0) ; 1
(index test 3) ; 4
(length test) ; 4
(length (list )) ; 0

(define odd (list 1 3 5 7 9))
(define even (list 0 2 4 6 8))
(append odd even)
(append even odd)

(map (lambda (x) (* x 2)) (list 1 2 3) ) ; (2 4 6)

(define 1-to-5 (list 1 2 3 4 5))
(for-each (lambda (x) (newline) (display x)) 1-to-5)

(filter odd? (list 1 2 3 4 5)) 

(reduce + 0 (list 1 2 3 4 5)) ; 15
(reduce * 1 (list 1 2 3 4 5)) ; 120
(reduce * 1 (list 1 2 3 4 5)) ; 120
(reduce cons '() (list 1 2 3)) ; 1 2 3

(range 0 10) ; (0 1 2 3 4 5 6 7 8 9 10)

(define (prime-sum? seq)
  (prime? (+ (car seq) (cadr seq))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum?
            (flatmap
                (lambda (i)
                    (map (lambda (j) (list i j))
                        (range 1 (- i 1))))
                (range 1 n)))))
(prime-sum-pairs 7)
; => ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11) (7 4 11) (7 6 13))

(flatmap
    (lambda (i)
      (map (lambda (j) (list i j)) (range 1 (- i 1))))
    (range 1 5))
; '((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))

(memq 'apple '(banana (apple orange) (pear (apple melon)) apple pineapple))
; '(apple pinnapple)