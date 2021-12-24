#lang racket
(provide deriv)

; Expressions: 
; dx/dx = 1
; d(u + v)/dx = du/dx + dv/dx
; d(uv)/dx = u(du)/dx + v(dv)/dx

; Evaluates the expression
(define (deriv expr var)
    (cond ((number? expr) 0)
        ; dx/dx 
        ((variable? expr) (if (same-variable? expr var) 1 0))
        
        ((sum? expr)
            (make-sum 
                (deriv (addend expr) var)
                (deriv (augend expr) var)))
        
        ((product? expr)
            (make-sum
                (make-product (multiplier expr) (deriv (multiplicand expr) var))
                (make-product (deriv (multiplier expr) var) (multiplicand expr))))
        
        (else 
            (error "unknown expression type: DERIV" expr))))

; Is e a variable? 
(define (variable? e) (symbol? e))
; Are v1 and v2 the same variable?
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
; Checked if expression is a number and the given number
(define (=number? expr num) (and (number? expr) (= expr num)))
; Construct the sum of a1 and a2 expressed in list.
(define (make-sum a1 a2) 
    (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
; Construct the product of m1 and m2.
(define (make-product a1 a2) 
    (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list '* a1 a2))))
; Is e a sum? A list and starts with '+
(define (sum? e) (and (pair? e) (eq? (car e) '+)))
; Is e a product?
(define (product? e) (and (pair? e) (eq? (car e) '*)))
; Addend of the sum e. Second item. 
(define (addend e) (cadr e))
; Augend of the sum e. Third item. 
(define (augend e) (caddr e))
; Multiplier of the product e. 
(define (multiplier e) (cadr e))
; Multiplicand of the product e. 
(define (multiplicand e) (caddr e))
