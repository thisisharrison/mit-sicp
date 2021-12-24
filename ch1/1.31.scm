(define (product a term b next)
    (if (> a b)
        1
        (* (term a)
            (product (next a) term b next))))

; product of integers
(define (inc a) (+ a 1))
(define (identity a) a)
(define (product-int a b)
    (product a identity b inc))
(product-int 1 5) ; 1 * 2 * 3 * 4 * 5
;Value: 120

(define (product-iter a term b next)
    (define (iter a res)
        ; finish when a is greater than b
        (if (> a b) 
            res
            ; update a and result (term a * running result)
            (iter (next a) (* (term a) res))))
    ; run this iter function
    (iter a 1))
(product-iter 1 identity 5 inc)
;Value: 120
