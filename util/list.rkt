#lang racket
(provide 
  index 
  length 
  append 
  map 
  for-each 
  range 
  filter 
  reduce 
  flatmap 
  memq 
  find
  exists?
  intersection
)

; index
(define (index items idx)
  (if (= idx 0)
    ; index 0, car of items (first!)
    (car items)
    ; otherwise cdr (n - 1) items, and decrement index
    (index (cdr items) (- idx 1))))

; length
(define (length items)
  ; items containing empty list
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

; append
(define (append l1 l2)
  ; we'll empty l1, so if l1 is null, return l2 
  (if (null? l1)
    l2
    (cons (car l1) (append (cdr l1) l2))))

; map 
(define (map proc li)
  (if (null? li) 
    '() ; nil
    (cons (proc (car li)) 
          (map proc (cdr li)))))

; for each 
(define (for-each proc list)
    (cond ((null? list) "done")
      ; do it to the first of the list
      (else (proc (car list))
            ; do it to the rest of the list
            (for-each proc (cdr list)))))

; range
(define (range low high)
  (if (> low high) '()
    (cons low (range (+ low 1) high))))

; filter
(define (filter predicate seq)
  (cond ((null? seq) '())
    ((predicate (car seq)) (cons (car seq) (filter predicate (cdr seq))))
    (else (filter predicate (cdr seq)))))

; reduce
(define (reduce proc init seq)
  (if (null? seq) 
    init
    (proc (car seq) 
      (reduce proc init (cdr seq)))))

; flatmap
(define (flatmap proc seq)
  (reduce append '() (map proc seq)))

; memq
(define (memq item seq)
  (cond ((null? seq) #f)
    ; returns sublist beginning with first occurrence 
    ((eq? (car seq) item) seq)
    (else (memq item (cdr seq)))))

;; TODO add example cases

; find: returns the element
(define (find item seq)
  (cond ((null? seq) '())
    ((equal? (car seq) item) item)
    (else (find item (cdr seq)))))

; exists?: returns #t if found, #f if not
(define (exists? item seq)
  (cond ((null? seq) #f)
    ((equal? (car seq) item) #t)
    (else (find item (cdr seq)))))

; intersection: returns list of common elements
(define (intersection l1 l2)
  (cond ((or (null? l1) (null? l2)) '())
    ((exists? (car l1) l2)
      (cons (car l1) (intersection (cdr l1) l2))) 
    (else (intersection (cdr l1) l2))))
