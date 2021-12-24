#lang sicp

(define my-list
  (cons 1
    (cons 2
      (cons 3
        ; should be nil instead of 0
        (cons 4 0)))))

(car my-list) ; 1
(cdr my-list) ; (2 3 4 . 0)
(car (cdr my-list)) ; 2

; this is equivalent to above! 
(list 1 2 3 4) ; (1 2 3 4)

(define one-to-four (list 1 2 3 4))
(car one-to-four) ; 1
(cdr (cdr (cdr one-to-four))) ; 2 3 4 then 3 4 then 4

(car (cdr one-to-four)) ; 2
(cadr one-to-four) ; 2, short hand for above

(cons 5 one-to-four) ; makes a list like original with 5 in the beginning (5 1 2 3 4)

; indexing
(define (index items idx)
  (if (= idx 0)
    ; index 0, car of items (first!)
    (car items)
    ; otherwise cdr (n - 1) items, and decrement index
    (index (cdr items) (- idx 1))))

(index one-to-four 0) ; 1
(index one-to-four 3) ; 4

; length
(define (length items)
  ; items containing empty list
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(length one-to-four) ; 4
(define nil-list (list))
(length nil-list) ; 0

(define (len items)
  (define (length items count)
    (if (null? items)
      count
      (length (cdr items) (+ 1 count))))
  (length items 0))

(len one-to-four) ; 4
(len nil-list) ; 0

; append
(define (append l1 l2)
  ; we'll empty l1, so if l1 is null, return l2 
  (if (null? l1)
    l2
    (cons (car l1) (append (cdr l1) l2))))

(define odd (list 1 3 5 7 9))
(define even (list 0 2 4 6 8))

(append odd even)
(append even odd)

; append iter
(define (join l1 l2)
  (define (reverse-append rev app)
    (if (null? rev)
        app
        (reverse-append (cdr rev) 
                        (cons (car rev) app))))
  (reverse-append (reverse l1) l2))   
; (cdr rev)     (cons (car rev) app)
; 9 7 5 3 1     0 2 4 6 8
; 7 5 3 1       9 0 2 4 6 8
; 5 3 1         7 9 0 2 4 6 8
; 3 1           5 7 9 0 2 4 6 8
; 1             3 5 7 9 0 2 4 6 8
; nil           1 3 5 7 9 0 2 4 6 8

(join odd even)
(join even odd)

; general map
(map + (list 1 2 3) (list 10 20 30)) ; (11 22 33)

; map 
(define (my-map li proc)
  (if (null? li) 
    nil ; nil
    (cons (proc (car li)) 
          (my-map (cdr li) proc))))
(my-map (list 1 2 3) (lambda (x) (* x 2))) ; (2 4 6)

; for each 
(define (for-each proc list)
    (cond ((null? list) "done")
      ; do it to the first of the list
      (else (proc (car list))
            ; do it to the rest of the list
            (for-each proc (cdr list)))))

(define 1-to-5 (list 1 2 3 4 5))
(for-each (lambda (x) (newline) (display x)) 1-to-5)
; 1
; 2
; 3
; 4
; 5
; ;Value: "done"
