; TODO: Add example file

;; Sets as Unordered Lists

; Element inside of set
; O(n)
(define (element-of-set? x set)
    (cond ((null? set) '())
        ((equal? (car set) x) #t)
        (else (element-of-set? x (cdr set)))))

; Intersection of set
; O(n ^ 2) - complete scan be s2 for each element of s1
(define (intersection-set s1 s2)
    (cond ((or (null? s1) (null? s2)) '())
        ((element-of-set? (car s1) s2)
            ; find the rest
            (cons (car s1) 
                    (intersection-of-set (cdr s1) s2)))
        (else (intersection-of-set (cdr s1) s2))))

;; Sets as Ordered Lists
; Compare with < and >

; Element inside of a set
; If element we're finding is smaller than first element of set, then false
; O(n), average O(n / 2)
(define (element-of-ordered-set? x set)
    (cond ((null? set) '())
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else element-of-ordered-set? x (cdr set))))

; Intersection of set
; If x < y,  cdr s1
; If x > y, cdr s2
(define (intersection-ordered-set s1 s2)
    (if (or (null? s1) (null? s2)) 
        '()
        (let ((x (car s1))) ((y (car s2)))
            (cond ((= x y) (cons x (intersection-ordered-set (cdr s1) (cdr s2))))
                ((< x y) (intersection-ordered-set (cdr s1) s2))
                ; x > y so cdr s2 to find a bigger y
                (else (intersection-ordered-set s1 (cdr s2)))))))

; Set as Binary Tree (BST)
; Represented by (root, left, right)
(define (root tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))
(define (new-tree root left right) (list root left right))

; Element of set
; O(log n) -> If traversed to a leaf node, element not exist
(define (element-in-bst x tree)
    (cond ((null? tree) #f)
        ((= x (root tree)) #t)
        ((< x (root tree)) (element-in-bst x (left tree)))
        ((> x (root tree)) (element-in-bst x (right tree)))))

; Adjoin set 
; O(log n) -> traverse to find where to add element
(define (adjoin-bst x tree)
    (cond ((null? tree) (new-tree x '() '()))
        ; not allow duplicates in set
        ((= x (root tree)) tree)
        ((< x (root tree))
            (make-tree (root tree)
                        ; x is smaller than root, adjoin x with left
                        (adjoin-bst x (left tree))
                        (right tree)))
        ((> x (root tree))
            (make-tree (root tree)
                        (left tree)
                        (adjoin-bst x (right tree))))))