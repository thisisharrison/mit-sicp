;; Huffman Encoding Tree
#lang racket
(require (submod "../../util/list.rkt") (submod "../../util/general.rkt"))

; creating and reading the nodes
; first in the list is a symbol called 'leaf'
(define (make-leaf symbol weight) (list 'leaf symbol weight))
; check by checking first symbol
(define (leaf? node) (eq? (car node) 'leaf))
; getter of symbol
(define (symbol-leaf node) (print "@@SYMBOL-LEAF") (cadr node))
; getter of weight
(define (weight-leaf node) (caddr node))

; tree shape is list of left, right, symbols, weight
; {({ABCDEFGH}17)}
(define (make-tree left right)
    (print "@@MAKE-TREE")
    (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; selectors for getting the symbols and weight 
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
    (if (leaf? tree)
        ; use leaf node method if leaf
        (list (symbol-leaf tree))
        ; otherwise stored in index 2
        (caddr tree)))
(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        ; index 3
        (cadddr tree)))


; adjoin in order of weight and elements is never already in set
(define (adjoin-set res set)
    (cond ((null? set) (list res))
        ; result's weight < first in set, put it in the back
        ((< (weight res) (weight (car set))) (cons res set))
        ; result's weight > first in set, put first in set in the front
        (else (cons (car set) 
                    (adjoin-set res (cdr set))))))

; use adjoin to make leaf set
(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair)
                                    (cadr pair))
                        (make-leaf-set (cdr pairs))))))

(define sample-tree 
    (make-tree 
        (make-leaf 'A 4)
            (make-tree
                (make-leaf 'B 2)
                    (make-tree
                        (make-leaf 'D 1)
                            (make-leaf 'C 1)))))

(print sample-tree)
; ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)

(define (decode bits tree)
    (define (decode-iter bits current-branch)
        (print "@@DECODE-ITER")
        (if (null? bits)
            '()
            (let ((next-branch
                    (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch)
                            (decode-iter (cdr bits) tree))
                    (decode-iter (cdr bits) next-branch)))))
    (decode-iter bits tree))

; go left if 0, right if 1
(define (choose-branch bit branch)
    (print "@@CHOOSE-BRANCH")
    (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(print (decode sample-message sample-tree))
; (A D A B B C A)