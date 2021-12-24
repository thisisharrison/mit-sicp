#lang sicp

;; prime?
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

(define (prime-sum? seq)
  (prime? (+ (car seq) (cadr seq))))

(define (make-pair-sum pair)
; extract car and car of cdr then 3rd item is sum of both
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; accummulate
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;; range
(define (range low high)
  (if (> low high)
    nil
    (cons low (range (+ 1 low) high))))


;; filter
(define (filter predicate seq)
    (cond ((null? seq) nil)
        ((predicate (car seq)) (cons (car seq) (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

(filter (lambda (x) (= x 2)) (list 1 2 3))

;; prime-sum-pairs
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
; => ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))

;; in python
; for i in range(1, 5):
;   for i in range(1, i - 1):

;; so it starts with range(1, 0), no op
; (2, 1), then (3, 1 to 2), (4, 1 to 3), (n, 1 to n - 1)

;; in javascript
; [1,2,3,4,5].reduce((acc, cur) => {
;   for (let i = 1; i < cur; i++) {
;     acc.push([cur, i])
;   }
;   return acc
; }, [])
; (2, 1), then (3, 1 to 2), (4, 1 to 3), (n, 1 to n - 1)

(accumulate max 0
  (map (lambda (x) (* x 10))
    (filter (lambda (x) (< x 10))
      (list 2 3 7 6 5 4 8 9 5 6 10))))
; => 90

(define (for-each seq proc) 
  (cond ((null? seq) "done")
    ; execute the first element
    (else (proc (car seq))
      ; and the rest
      (for-each (cdr seq) proc))))

(for-each (range 1 5)
  (lambda (i) 
    (for-each (range i 5)
      (lambda (j) 
        (display (list i j))))
    (newline)))
; (1 1)(1 2)(1 3)(1 4)(1 5)
; (2 2)(2 3)(2 4)(2 5)
; (3 3)(3 4)(3 5)
; (4 4)(4 5)
; (5 5)

; in python: 
; for i in range(1, 5):
;   for j in range(i, 5):
;     print i j

;; flatmap 
; in javascript 
; [1,2,3,4,5].reduce((acc, cur) => acc.concat([cur, cur * 2]), [])
;; a 2D array returned to 1D
; [1, 2, 2, 4, 3, 6, 4, 8, 5, 10]

; now it's one list with sublists
(define (nested n)
  (accumulate append nil 
    (map (lambda (i)
      (map (lambda (j) (list i j)) (range 1 i)))
        (range 1 n))))
(nested 5)
; => ((1 1) (2 1) (2 2) (3 1) (3 2) (3 3) (4 1) (4 2) (4 3) (4 4) (5 1) (5 2) (5 3) (5 4) (5 5))
; before we do append with the rest, map the seq first
(define (flat-map proc seq)
  (accumulate append nil (map proc seq)))

;; same as before but using flat-map
(define (nested2 n)
  (flat-map
    (lambda (i)
      (map (lambda (j) (list i j)) (range 1 i)))
        (range 1 n)))
(nested2 5)
; => ((1 1) (2 1) (2 2) (3 1) (3 2) (3 3) (4 1) (4 2) (4 3) (4 4) (5 1) (5 2) (5 3) (5 4) (5 5))

