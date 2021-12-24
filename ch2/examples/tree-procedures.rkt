#lang sicp

(define (print sth)
  (display sth)
  (newline))

(define (leaf? node)
  (not (pair? node)))

(define sample (cons (list 1 2) (list 3 4)))
(print sample) ; ((1 2) 3 4)

(define nest-sample (list sample sample))
(print nest-sample) ; (((1 2) 3 4) ((1 2) 3 4)) 

; length (count leaves)
(define (count-leaves tree)
  ; count-leaves of empty tree is 0
  (cond ((null? tree) 0)
    ((leaf? tree) 1)
    ; count-leaves of a tree is count-leaves of car tree + cdr tree
    (else (+ (count-leaves (car tree)) 
          (count-leaves (cdr tree))))))

(print (count-leaves sample)) ; 4
(print (count-leaves nest-sample)) ; 8

; map
(define (my-map tree proc)
  (cond ((null? tree) nil)
        ((leaf? tree) (proc tree))
        (else (cons (my-map (car tree) proc) (my-map (cdr tree) proc)))
  )
)
(print (my-map sample (lambda (x) (* x 2)))) ; ((2 4) 6 8)
(print (my-map nest-sample (lambda (x) (* x 2)))) ; (((2 4) 6 8) ((2 4) 6 8))

; using lisp's general map - (map + l1 l2) will return l1 + l2 
(define (lisp-map tree proc)
  (map (lambda (sub-tree)
              (if (leaf? sub-tree) 
                  (proc sub-tree)
                  (lisp-map sub-tree proc)))
        tree))
(print (lisp-map sample (lambda (x) (* x 2)))) ; ((2 4) 6 8)
(print (lisp-map nest-sample (lambda (x) (* x 2)))) ; (((2 4) 6 8) ((2 4) 6 8))

; filter
(define (filter predicate sequence) 
    (cond ((null? sequence) nil)
        ((predicate (car sequence))
            (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5)) 

; accummulate (reduce)
(define (accummulate proc init sequence)
    (if (null? sequence) 
        init
        (proc (car sequence)
            (accummulate proc init (cdr sequence)))))

(accummulate + 0 (list 1 2 3 4 5)) ; 15
(accummulate * 1 (list 1 2 3 4 5)) ; 120

; range 
(define (range low high)
    (if (> low high) 
        nil
        (cons low (range (+ 1 low) high))))
(range 0 10) ; (0 1 2 3 4 5 6 7 8 9 10)

(define (square x) (* x x))

(define (list-even-square n)
    (accummulate
        cons
        nil
        (map square (filter even? (range 0 n)))
        ))
(list-even-square 10) ; (0 4 16 36 64 100)



; TODO
; pre-order
; in-order
; post-order