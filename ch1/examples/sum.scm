(define (sum a term b next)
    (if (> a b)
        0
        (+ (term a) 
            (sum (next a) term b next))))

; sum of integers
(define (inc a) (+ a 1))
(define (identity a) a)
(define (sum-int a b)
    (sum a identity b inc))
(sum-int 1 10)
;Value: 55

; sum of cubes (using lambda)
(define (sum-cubes a b)
    (sum a (lambda (a) (* a a a)) b inc))
(sum-cubes 1 10)
;Value: 3025

